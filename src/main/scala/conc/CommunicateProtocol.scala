package conc

import conc.Chapter10._
import io.reactors._
import io.reactors.protocol
import scala.language.{higherKinds, implicitConversions}

trait CommunicateProtocol[MsgType] {
  type ConnectInfo
  def connectInfo: ConnectInfo
  def sendAsync(m: MsgType): Unit
  def sendSync(m: MsgType): MsgType
  def onReceive(m: MsgType): MsgType
}

