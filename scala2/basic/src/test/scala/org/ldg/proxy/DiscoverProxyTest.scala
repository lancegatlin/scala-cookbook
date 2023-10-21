package org.ldg.proxy

import java.net.InetSocketAddress
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiscoverProxyTest extends AnyFlatSpec with Matchers {

  "discoverProxies" should "discover proxies" in {
    val fixture = new DiscoverProxies
    val proxies = fixture.discoverProxies()
    proxies match {
      case Nil => println("No proxies")
      case _ =>
        for (proxy <- proxies) yield {
          println("proxy hostname : " + proxy.`type`())
          val addr = proxy.address().asInstanceOf[InetSocketAddress]

          if (addr == null) {
            println("Null Proxy")
          } else {
            println(s"proxy hostname : ${addr.getHostName}")
            println(s"proxy port : ${addr.getPort}")
          }
        }
    }
  }
}
