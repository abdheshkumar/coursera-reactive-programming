package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    doOr(in1, in2, out)
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    doOr(in1, in2, out)
  }

  def doOr(in1: Wire, in2: Wire, out: Wire) {
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false)

    in1.setSignal(true)
    in2.setSignal(false)
    run
    assert(out.getSignal === true)

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true)

    in1.setSignal(true)
    in2.setSignal(true)
    run
    assert(out.getSignal === true)
  }

  test("demux") {
    val in = new Wire
    val c0 = new Wire
    val c1 = new Wire
    val c = List(c0, c1)
    val o0 = new Wire
    val o1 = new Wire
    val o2 = new Wire
    val o3 = new Wire
    val out = List(o0, o1, o2, o3)
    demux(in, c, out)

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === true)

    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(false)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === true)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === true)
    assert(o3.getSignal === false)

    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(o0.getSignal === true)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(true)
    c1.setSignal(false)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(true)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(true)
    c1.setSignal(true)
    checkAllFalse(out)
  }

  def checkAllFalse(out: List[Wire]) {
    run
    out.foreach(a => assert(a.getSignal === false))
  }
}
