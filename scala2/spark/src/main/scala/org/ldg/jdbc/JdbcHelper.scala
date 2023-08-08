package org.ldg.jdbc

trait JdbcHelper {
  def jdbcConfig: JdbcConfig

  def quote(identifier: String) : String =
    if(identifier.startsWith(""""""") == false) {
      s""""$identifier""""
    } else {
      identifier
    }
}
