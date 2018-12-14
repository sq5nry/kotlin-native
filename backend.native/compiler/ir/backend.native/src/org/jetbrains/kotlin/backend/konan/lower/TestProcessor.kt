/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package org.jetbrains.kotlin.backend.konan.lower

import org.jetbrains.kotlin.backend.common.descriptors.WrappedClassConstructorDescriptor
import org.jetbrains.kotlin.backend.common.descriptors.WrappedClassDescriptor
import org.jetbrains.kotlin.backend.common.descriptors.WrappedSimpleFunctionDescriptor
import org.jetbrains.kotlin.backend.common.lower.SymbolWithIrBuilder
import org.jetbrains.kotlin.backend.common.lower.createIrBuilder
import org.jetbrains.kotlin.backend.common.reportWarning
import org.jetbrains.kotlin.backend.konan.KonanBackendContext
import org.jetbrains.kotlin.backend.konan.descriptors.isAbstract
import org.jetbrains.kotlin.backend.konan.descriptors.synthesizedName
import org.jetbrains.kotlin.backend.konan.irasdescriptors.fqNameSafe
import org.jetbrains.kotlin.backend.konan.irasdescriptors.typeWithStarProjections
import org.jetbrains.kotlin.backend.konan.irasdescriptors.typeWithoutArguments
import org.jetbrains.kotlin.backend.konan.reportCompilationError
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.builders.*
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrClassImpl
import org.jetbrains.kotlin.ir.declarations.impl.IrConstructorImpl
import org.jetbrains.kotlin.ir.declarations.impl.IrFunctionImpl
import org.jetbrains.kotlin.ir.expressions.impl.*
import org.jetbrains.kotlin.ir.symbols.*
import org.jetbrains.kotlin.ir.symbols.impl.IrClassSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrConstructorSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.classifierOrNull
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.ir.visitors.IrElementVisitorVoid
import org.jetbrains.kotlin.ir.visitors.acceptChildrenVoid
import org.jetbrains.kotlin.load.kotlin.PackagePartClassUtils
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.types.KotlinType

internal class TestProcessor (val context: KonanBackendContext) {

    object TEST_SUITE_CLASS: IrDeclarationOriginImpl("TEST_SUITE_CLASS")
    object TEST_SUITE_GENERATED_MEMBER: IrDeclarationOriginImpl("TEST_SUITE_GENERATED_MEMBER")

    companion object {
        val COMPANION_GETTER_NAME = Name.identifier("getCompanion")
        val INSTANCE_GETTER_NAME = Name.identifier("createInstance")

        val IGNORE_FQ_NAME = FqName.fromSegments(listOf("kotlin", "test" , "Ignore"))
    }

    val symbols = context.ir.symbols

    private val topLevelSuiteNames = mutableSetOf<String>()

    // region Useful extensions.
    var testSuiteCnt = 0
    fun Name.synthesizeSuiteClassName() = identifier.synthesizeSuiteClassName()
    fun String.synthesizeSuiteClassName() = "$this\$test\$${testSuiteCnt++}".synthesizedName

    // IrFile always uses a forward slash as a directory separator.
    private val IrFile.fileName
        get() = name.substringAfterLast('/')
    private val IrFile.topLevelSuiteName: String
        get() {
            val packageFqName = packageFragmentDescriptor.fqName
            val shortFileName = PackagePartClassUtils.getFilePartShortName(fileName)
            return if (packageFqName.isRoot) shortFileName else "$packageFqName.$shortFileName"
        }

    private fun MutableList<TestFunction>.registerFunction(
            function: IrFunctionSymbol,
            kinds: Collection<FunctionKind>) = kinds.forEach { add(TestFunction(function, it)) }

    private fun MutableList<TestFunction>.registerFunction(function: IrFunctionSymbol, kind: FunctionKind) =
            add(TestFunction(function, kind))

    private fun <T: IrElement> IrStatementsBuilder<T>.generateFunctionRegistration(
            receiver: IrValueDeclaration,
            registerTestCase: IrFunctionSymbol,
            registerFunction: IrFunctionSymbol,
            functions: Collection<TestFunction>) {
        functions.forEach {
            if (it.kind == FunctionKind.TEST) {
                // Call registerTestCase(name: String, testFunction: () -> Unit) method.
                +irCall(registerTestCase, registerTestCase.descriptor.returnType!!.toErasedIrType()).apply {
                    dispatchReceiver = irGet(receiver)
                    putValueArgument(0, IrConstImpl.string(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            context.irBuiltIns.stringType,
                            it.function.descriptor.name.identifier)
                    )
                    putValueArgument(1, IrFunctionReferenceImpl(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            descriptor.valueParameters[1].type.toErasedIrType(),
                            it.function,
                            it.function.descriptor, 0))
                    putValueArgument(2, IrConstImpl.boolean(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            context.irBuiltIns.booleanType,
                            it.ignored
                    ))
                }
            } else {
                // Call registerFunction(kind: TestFunctionKind, () -> Unit) method.
                +irCall(registerFunction, registerFunction.descriptor.returnType!!.toErasedIrType()).apply {
                    dispatchReceiver = irGet(receiver)
                    val testKindEntry = it.kind.runtimeKind
                    putValueArgument(0, IrGetEnumValueImpl(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            symbols.testFunctionKind.typeWithoutArguments,
                            testKindEntry)
                    )
                    putValueArgument(1, IrFunctionReferenceImpl(UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            descriptor.valueParameters[1].type.toErasedIrType(),
                            it.function,
                            it.function.descriptor, 0))
                }
            }
        }
    }
    // endregion

    // region Classes for annotation collection.
    internal enum class FunctionKind(annotationNameString: String, runtimeKindString: String) {
        TEST("kotlin.test.Test", "") {
            override val runtimeKindName: Name  get() = throw NotImplementedError()
        },

        BEFORE_EACH("kotlin.test.BeforeEach", "BEFORE_EACH"),
        AFTER_EACH("kotlin.test.AfterEach", "AFTER_EACH"),
        BEFORE_CLASS("kotlin.test.BeforeClass", "BEFORE_CLASS"),
        AFTER_CLASS("kotlin.test.AfterClass", "AFTER_CLASS");

        val annotationFqName = FqName(annotationNameString)
        open val runtimeKindName = Name.identifier(runtimeKindString)

        companion object {
            val INSTANCE_KINDS = listOf(TEST, BEFORE_EACH, AFTER_EACH)
            val COMPANION_KINDS = listOf(BEFORE_CLASS, AFTER_CLASS)
        }
    }

    private val FunctionKind.runtimeKind: IrEnumEntrySymbol
        get() = symbols.getTestFunctionKind(this)

    private data class TestFunction(val function: IrFunctionSymbol, val kind: FunctionKind) {
        val ignored: Boolean
            get() = function.descriptor.annotations.hasAnnotation(IGNORE_FQ_NAME)
    }

    private inner class TestClass(val ownerClass: IrClassSymbol) {
        var companion: IrClassSymbol? = null
        val functions = mutableListOf<TestFunction>()

        fun registerFunction(function: IrFunctionSymbol, kinds: Collection<FunctionKind>) =
                functions.registerFunction(function, kinds)
        fun registerFunction(function: IrFunctionSymbol, kind: FunctionKind) =
                functions.registerFunction(function, kind)
    }

    private inner class AnnotationCollector(val irFile: IrFile) : IrElementVisitorVoid {
        val testClasses = mutableMapOf<IrClassSymbol, TestClass>()
        val topLevelFunctions = mutableListOf<TestFunction>()

        private fun MutableMap<IrClassSymbol, TestClass>.getTestClass(key: IrClassSymbol) =
                getOrPut(key) { TestClass(key) }

        private fun MutableMap<IrClassSymbol, TestClass>.getTestClass(key: ClassDescriptor) =
                getTestClass(symbols.symbolTable.referenceClass(key))

        override fun visitElement(element: IrElement) {
            element.acceptChildrenVoid(this)
        }

        fun IrFunctionSymbol.hasAnnotation(fqName: FqName) = descriptor.annotations.any { it.fqName == fqName }

        fun registerClassFunction(classDescriptor: ClassDescriptor,
                                  function: IrFunctionSymbol,
                                  kinds: Collection<FunctionKind>) {

            fun warn(msg: String) = context.reportWarning(msg, irFile, function.owner)

            kinds.forEach { kind ->
                val annotation = kind.annotationFqName
                when (kind) {
                    in FunctionKind.INSTANCE_KINDS -> with(classDescriptor) {
                        when {
                            isInner ->
                                warn("Annotation $annotation is not allowed for methods of an inner class")
                            isAbstract() ->
                                warn("Annotation $annotation is not allowed for methods of an abstract class")
                            isCompanionObject ->
                                warn("Annotation $annotation is not allowed for methods of a companion object")
                            constructors.none { it.valueParameters.size == 0 } ->
                                warn("Test class has no default constructor: ${fqNameSafe}")
                            else ->
                                testClasses.getTestClass(classDescriptor).registerFunction(function, kind)
                        }
                    }
                    in FunctionKind.COMPANION_KINDS ->
                        when {
                            classDescriptor.isCompanionObject -> {
                                val containingClass = classDescriptor.containingDeclaration as ClassDescriptor
                                val testClass = testClasses.getTestClass(containingClass)
                                testClass.companion = symbols.symbolTable.referenceClass(classDescriptor)
                                testClass.registerFunction(function, kind)
                            }
                            classDescriptor.kind == ClassKind.OBJECT -> {
                                testClasses.getTestClass(classDescriptor).registerFunction(function, kind)
                            }
                            else -> warn("Annotation $annotation is only allowed for methods of an object " +
                                    "(named or companion) or top level functions")

                        }
                    else -> throw IllegalStateException("Unreachable")
                }
            }
        }

        fun IrFunction.checkFunctionSignature() {
            // Test runner requires test functions to have the following signature: () -> Unit.
            if (descriptor.returnType != context.builtIns.unitType) {
                context.reportCompilationError(
                        "Test function must return Unit: ${descriptor.fqNameSafe}", irFile, this
                )
            }
            if (descriptor.valueParameters.isNotEmpty()) {
                context.reportCompilationError(
                        "Test function must have no arguments: ${descriptor.fqNameSafe}", irFile, this
                )
            }
        }

        // TODO: Use symbols instead of containingDeclaration when such information is available.
        override fun visitFunction(declaration: IrFunction) {
            val symbol = declaration.symbol
            val owner = declaration.descriptor.containingDeclaration

            val kinds = FunctionKind.values().filter { symbol.hasAnnotation(it.annotationFqName)  }
            if (kinds.isEmpty()) {
                return
            }
            declaration.checkFunctionSignature()

            when (owner) {
                is PackageFragmentDescriptor -> topLevelFunctions.registerFunction(symbol, kinds)
                is ClassDescriptor -> registerClassFunction(owner, symbol, kinds)
                else -> UnsupportedOperationException("Cannot create test function $declaration (defined in $owner")
            }
        }
    }
    // endregion

    //region Symbol and IR builders

    /**
     * Builds a method in `[testSuite]` class with name `[getterName]`
     * returning a reference to an object represented by `[objectSymbol]`.
     */
    private fun buildObjectGetter(objectSymbol: IrClassSymbol, owner: IrClass, getterName: Name)
        : IrFunction {

        val symbol = IrSimpleFunctionSymbolImpl(WrappedSimpleFunctionDescriptor())
        return IrFunctionImpl(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                TEST_SUITE_GENERATED_MEMBER,
                symbol,
                getterName,
                Visibilities.PROTECTED,
                Modality.FINAL,
                objectSymbol.typeWithStarProjections,
                isInline = false,
                isExternal = false,
                isTailrec = false,
                isSuspend = false
        ).apply {
            (descriptor as WrappedSimpleFunctionDescriptor).bind(this)
            parent = owner

            val superFunction = symbols.baseClassSuite.owner.simpleFunctions()
                    .single { it.name == getterName && it.valueParameters.isEmpty() }

            createDispatchReceiverParameter()
            overriddenSymbols += superFunction.symbol

            body = context.createIrBuilder(symbol).irBlockBody {
                +irReturn(IrGetObjectValueImpl(UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                        objectSymbol.typeWithoutArguments, objectSymbol)
                )
            }
        }
    }

    /**
     * Builds a method in `[testSuite]` class with name `[getterName]`
     * returning a new instance of class referenced by [classSymbol].
     */
    private fun buildInstanceGetter(classSymbol: IrClassSymbol, owner: IrClass, getterName: Name)
        : IrFunction {

        val symbol = IrSimpleFunctionSymbolImpl(WrappedSimpleFunctionDescriptor())
        return IrFunctionImpl(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                TEST_SUITE_GENERATED_MEMBER,
                symbol,
                getterName,
                Visibilities.PROTECTED,
                Modality.FINAL,
                classSymbol.typeWithStarProjections,
                isInline = false,
                isExternal = false,
                isTailrec = false,
                isSuspend = false
        ).apply {
            (descriptor as WrappedSimpleFunctionDescriptor).bind(this)
            parent = owner

            val superFunction = symbols.baseClassSuite.owner.simpleFunctions()
                    .single { it.name == getterName && it.valueParameters.isEmpty() }

            createDispatchReceiverParameter()
            overriddenSymbols += superFunction.symbol

            body = context.createIrBuilder(symbol).irBlockBody {
                val constructor = classSymbol.owner.constructors.single { it.valueParameters.isEmpty() }
                +irReturn(irCall(constructor))
            }
        }
    }

    /**
     * Builds a constructor for a test suite class representing a test class (any class in the original IrFile with
     * method(s) annotated with @Test). The test suite class is a subclass of ClassTestSuite<T>
     * where T is the test class.
     */
    private fun buildClassSuiteConstructor(suiteName: String,
                                           testClassType: IrType,
                                           testCompanionType: IrType,
                                           testSuite: IrClassSymbol,
                                           owner: IrClass,
                                           functions: Collection<TestFunction>,
                                           ignored: Boolean) = WrappedClassConstructorDescriptor().let { descriptor ->
        IrConstructorImpl(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                TEST_SUITE_GENERATED_MEMBER,
                IrConstructorSymbolImpl(descriptor),
                Name.special("<init>"),
                Visibilities.PUBLIC,
                testSuite.typeWithStarProjections,
                isInline = false,
                isExternal = false,
                isPrimary = true
        ).apply {
            descriptor.bind(this)
            parent = owner

            fun IrClass.getFunction(name: String, predicate: (IrSimpleFunction) -> Boolean) =
                    simpleFunctions().single { it.name.asString() == name && predicate(it) }

            val registerTestCase = symbols.baseClassSuite.owner.getFunction("registerTestCase") {
                it.valueParameters.size == 3 &&
                it.valueParameters[0].type.classifierOrNull == context.irBuiltIns.stringClass && // name: String
                it.valueParameters[1].type.isFunction() &&           // function: testClassType.() -> Unit
                it.valueParameters[2].type.classifierOrNull == context.irBuiltIns.booleanClass   // ignored: Boolean
            }
            val registerFunction = symbols.baseClassSuite.owner.getFunction("registerFunction") {
                it.valueParameters.size == 2 &&
                it.valueParameters[0].type.classifierOrNull == symbols.testFunctionKind && // kind: TestFunctionKind
                it.valueParameters[1].type.isFunction()                                    // function: () -> Unit
            }

            body = context.createIrBuilder(symbol).irBlockBody {
                val superConstructor = symbols.baseClassSuiteConstructor
                +IrDelegatingConstructorCallImpl(
                        UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                        context.irBuiltIns.unitType,
                        symbols.symbolTable.referenceConstructor(superConstructor),
                        superConstructor,
                        typeArgumentsCount = 2,
                        valueArgumentsCount = 2
                ).apply {
                    putTypeArgument(0, testClassType)
                    putTypeArgument(1, testCompanionType)

                    putValueArgument(0, IrConstImpl.string(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            context.irBuiltIns.stringType,
                            suiteName)
                    )
                    putValueArgument(1, IrConstImpl.boolean(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            context.irBuiltIns.booleanType,
                            ignored
                    ))
                }
                generateFunctionRegistration(testSuite.owner.thisReceiver!!,
                        registerTestCase.symbol, registerFunction.symbol, functions)
            }
        }
    }

    private fun KotlinType.toErasedIrType(): IrType = context.ir.translateErased(this)

    private val IrClassSymbol.ignored: Boolean get() = descriptor.annotations.hasAnnotation(IGNORE_FQ_NAME)
    private val IrClassSymbol.isObject: Boolean get() = descriptor.kind == ClassKind.OBJECT

    /**
     * Builds a test suite class representing a test class (any class in the original IrFile with method(s)
     * annotated with @Test). The test suite class is a subclass of ClassTestSuite<T> where T is the test class.
     */
    private fun buildClassSuite(testClass: IrClassSymbol,
                                testCompanion: IrClassSymbol?,
                                functions: Collection<TestFunction>) = WrappedClassDescriptor().let { descriptor ->
        IrClassImpl(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                TEST_SUITE_CLASS,
                IrClassSymbolImpl(descriptor),
                testClass.owner.name.synthesizeSuiteClassName(),
                ClassKind.CLASS,
                Visibilities.PRIVATE,
                Modality.FINAL,
                isCompanion = false,
                isInner = false,
                isData = false,
                isExternal = false,
                isInline = false
        ).apply {
            descriptor.bind(this)
            createParameterDeclarations()

            val testClassType = testClass.owner.defaultType
            val testCompanionType = if (testClass.isObject) {
                testClassType
            } else {
                testCompanion?.owner?.defaultType ?: context.irBuiltIns.nothingType
            }

            val constructor = buildClassSuiteConstructor(
                    testClass.owner.fqNameSafe.toString(), testClassType, testCompanionType,
                    symbol, this, functions, testClass.ignored
            )

            val instanceGetterBuilder: IrFunction
            val companionGetterBuilder: IrFunction?

            if (testClass.isObject) {
                instanceGetterBuilder = buildObjectGetter(testClass, this, INSTANCE_GETTER_NAME)
                companionGetterBuilder = buildObjectGetter(testClass, this, COMPANION_GETTER_NAME)
            } else {
                instanceGetterBuilder = buildInstanceGetter(testClass, this, INSTANCE_GETTER_NAME)
                companionGetterBuilder = testCompanion?.let {
                    buildObjectGetter(it, this, COMPANION_GETTER_NAME)
                }
            }

            declarations += constructor
            declarations += instanceGetterBuilder
            companionGetterBuilder?.let { declarations += it }

            superTypes += symbols.baseClassSuite.typeWith(listOf(testClassType, testCompanionType))
            addFakeOverrides()
        }
    }
    //endregion

    // region IR generation methods
    private fun generateClassSuite(irFile: IrFile, testClass: TestClass) =
            with(buildClassSuite(testClass.ownerClass, testClass.companion,testClass.functions)) {
                irFile.addChild(this)
                val irConstructor = constructors.single()
                irFile.addTopLevelInitializer(
                        IrCallImpl(UNDEFINED_OFFSET, UNDEFINED_OFFSET, irConstructor.returnType, irConstructor.symbol),
                        context, threadLocal = true)
            }

    /** Check if this fqName already used or not. */
    private fun checkSuiteName(irFile: IrFile, name: String): Boolean {
        if (topLevelSuiteNames.contains(name)) {
            context.reportCompilationError("Package '${irFile.packageFragmentDescriptor.fqName}' has top-level test " +
                    "functions in several files with the same name: '${irFile.fileName}'")
            return false
        }
        topLevelSuiteNames.add(name)
        return true
    }

    private fun generateTopLevelSuite(irFile: IrFile, functions: Collection<TestFunction>) {
        val builder = context.createIrBuilder(irFile.symbol)
        val suiteName = irFile.topLevelSuiteName
        if (!checkSuiteName(irFile, suiteName)) {
            return
        }

        // TODO: an awful hack, we make this initializer thread local, so that it doesn't freeze suite,
        // and later on we could modify some suite's properties. This shall be redesigned.
        irFile.addTopLevelInitializer(builder.irBlock {
            val constructorCall = irCall(symbols.topLevelSuiteConstructor).apply {
                putValueArgument(0, IrConstImpl.string(UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                                context.irBuiltIns.stringType, suiteName))
            }
            val testSuiteVal = irTemporary(constructorCall,  "topLevelTestSuite")
            generateFunctionRegistration(testSuiteVal,
                    symbols.topLevelSuiteRegisterTestCase,
                    symbols.topLevelSuiteRegisterFunction,
                    functions)
        }, context, threadLocal = true)
    }

    private fun createTestSuites(irFile: IrFile, annotationCollector: AnnotationCollector) {
        annotationCollector.testClasses.filter {
            it.value.functions.any { it.kind == FunctionKind.TEST }
        }.forEach { _, testClass ->
            generateClassSuite(irFile, testClass)
        }
        if (annotationCollector.topLevelFunctions.isNotEmpty()) {
            generateTopLevelSuite(irFile, annotationCollector.topLevelFunctions)
        }
    }
    // endregion

    fun process(irModuleFragment: IrModuleFragment) {
        irModuleFragment.files.forEach {
            val annotationCollector = AnnotationCollector(it)
            it.acceptChildrenVoid(annotationCollector)
            createTestSuites(it, annotationCollector)
        }
    }
}