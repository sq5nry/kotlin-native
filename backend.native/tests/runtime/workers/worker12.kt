/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package runtime.workers.worker12

import kotlin.test.*

import kotlin.native.concurrent.*

@ThreadLocal
val map = mutableSetOf<Future>()

fun unprocessed() = map.size < 20

@Test fun runTest() {
    val workerProducer = Worker.start()
    val workerConsumer = Worker.start()

    workerConsumer.setFutureProcessor<String> {
        it ->
        map += it
        println("consumer got ${it.result}")
    }


    val futures = mutableSetOf<Future<String>>()
    repeat(20) {
        futures += workerProducer.execute(TransferMode.SAFE, { "Input" }) { input ->
            "$input processed"
        }.also { it.setFutureSubscriber(workerConsumer) }
    }

    while (workerConsumer.execute(TransferMode.SAFE, { null } { _ -> unprocessed() }).result == true) {
            waitForMultipleFutures(futures, 1000)
    }

    workerProducer.requestTermination(true)
    workerConsumer.requestTermination(true)
    println("OK")
}