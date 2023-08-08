package org.ldg.proxy

import java.net.Proxy
import java.net.URI


import com.github.markusbernhardt.proxy.ProxySearch
import com.github.markusbernhardt.proxy.selector.misc.BufferedProxySelector.CacheScope
import scala.collection.JavaConverters._

class DiscoverProxies {
    def discoverProxies(
        someUrl: String = "http://www.google.com/"
    ) : List[Proxy] = {
        System.setProperty("java.net.useSystemProxies","true")

        // Use proxy vole to find the default proxy
        val ps = ProxySearch.getDefaultProxySearch
        ps.setPacCacheSettings(32, 1000*60*5, CacheScope.CACHE_SCOPE_URL)

        Option(ps.getProxySelector).map(
            _.select(
                new URI(someUrl)
            ).asScala.toList
        ).getOrElse(Nil)
    }
}
