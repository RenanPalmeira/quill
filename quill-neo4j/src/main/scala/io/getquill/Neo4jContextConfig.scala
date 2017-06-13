package io.getquill

import com.typesafe.config.Config
import org.neo4j.driver.v1.{ GraphDatabase, Driver }

case class Neo4jContextConfig(config: Config) {
  def driver: Driver = GraphDatabase.driver(config.getString("neo4j.url"))
  def session = driver.session
}