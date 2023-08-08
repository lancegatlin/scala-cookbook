package org.ldg.jdbc

trait JdbcConfig {
  def url: String
  def username: String
  def password: String
}

object JdbcConfig {
  case class JdbcConfigImpl(
    url: String,
    username: String,
    password: String
  ) extends JdbcConfig

  def apply(
    url: String,
    username: String,
    password: String
  ) : JdbcConfig = JdbcConfigImpl(
    url = url,
    username = username,
    password = password
  )
}