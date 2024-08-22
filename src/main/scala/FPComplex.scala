package ComplexModules
import Chisel.log2Ceil
import chisel3._
import FloatingPointDesigns.FPArithmetic._
//import SWFFT.FFT._
import scala.collection.mutable

import chisel3.util._
import chiseltest.RawTester.test
import chisel3.tester._
import java.io.PrintWriter

import FP_Modules.FloatingPointDesigns._
object FPComplex { // these are the complex FP modules

  class ComplexNum(bw: Int, name:Int) extends Bundle{
    val Re = UInt(bw.W)
    val Im = UInt(bw.W)
  }

  class ComplexIO(bw: Int, name:Int) extends Bundle{
    val in_a = Input(new ComplexNum(bw,name))
    val in_b = Input(new ComplexNum(bw,name))
    val out_s = Output(new ComplexNum(bw,name))
  }

  class FPComplexAdder_v2(bw: Int, name:Int) extends Module {
    override def desiredName = s"FPComplexAdder_v2_${name}"
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })
    val FP_adders = (for (i <- 0 until 2) yield {
      val fpadd = Module(new FP_adder_13ccs(bw,name)).io
      fpadd
    }).toVector
    FP_adders(0).in_en := io.in_en
    FP_adders(1).in_en := io.in_en
    FP_adders(0).in_a := io.in_a.Re
    FP_adders(0).in_b := io.in_b.Re
    FP_adders(1).in_a := io.in_a.Im
    FP_adders(1).in_b := io.in_b.Im
    io.out_s.Re := FP_adders(0).out_s
    io.out_s.Im := FP_adders(1).out_s
  }

  class FPComplexAdder_reducable_v2(bw: Int, sRe: Double, sIm: Double, add_reg: Boolean, name:Int) extends Module { //can reduce the adder count
        override def desiredName = s"FPComplexAdder_reducable_v2_${name}"

    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })
    if (sRe.abs < 0.00005) { // if inout a is zero
      if (add_reg) { // we have the option of adding a register to maintain the pipeline
        val result = RegInit(0.U.asTypeOf(new ComplexNum(bw,name)))
        when(io.in_en){
          result := io.in_b
          io.out_s := result
        }.otherwise{
          io.out_s := result
        }
      } else {
        io.out_s := io.in_b // combinational logic
      }
    } else if (sIm.abs < 0.00005) { // if input b is zero
      if (add_reg) {
        val result = RegInit(0.U.asTypeOf(new ComplexNum(bw,name)))
        when(io.in_en){
          result := io.in_a
          io.out_s := result
        }.otherwise{
          io.out_s := result
        }
      } else {
        io.out_s := io.in_a
      }
    } else { // none of the input a or input b magnitudes are zero
      val FP_adders = (for (i <- 0 until 2) yield {
        val fpadd = Module(new FP_adder_v2(bw)).io
        fpadd
      }).toVector
      FP_adders(0).in_en := io.in_en
      FP_adders(1).in_en := io.in_en
      FP_adders(0).in_a := io.in_a.Re
      FP_adders(0).in_b := io.in_b.Re
      FP_adders(1).in_a := io.in_a.Im
      FP_adders(1).in_b := io.in_b.Im
      io.out_s.Re := FP_adders(0).out_s
      io.out_s.Im := FP_adders(1).out_s
    }
  }

  class FPComplexSub_v2(bw: Int, name:Int) extends Module {
    override def desiredName = s"FPComplexSub_v2_${name}"
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })
    val FP_subbers = (for (i <- 0 until 2) yield {
      val fpsub = Module(new FP_subtractor_13ccs(bw,name)).io
      fpsub
    }).toVector
    FP_subbers(0).in_en := io.in_en
    FP_subbers(1).in_en := io.in_en
    FP_subbers(0).in_a := io.in_a.Re
    FP_subbers(0).in_b := io.in_b.Re
    FP_subbers(1).in_a := io.in_a.Im
    FP_subbers(1).in_b := io.in_b.Im
    io.out_s.Re := FP_subbers(0).out_s
    io.out_s.Im := FP_subbers(1).out_s
  }

  class FPComplexSub_reducable_v2(bw: Int, sRe: Double, sIm: Double, add_reg: Boolean, name:Int) extends Module {
    override def desiredName = s"FPComplexSub_reducable_v2_${name}"
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })
    if (sRe.abs < 0.00005) { // check if magnitude of input a is zero
      if (add_reg) {
        val result = RegInit(0.U.asTypeOf(new ComplexNum(bw,name)))
        when(io.in_en) {
          result.Re := (~io.in_b.Re(bw - 1)) ## io.in_b.Re(bw - 2, 0)
          result.Im := (~io.in_b.Im(bw - 1)) ## io.in_b.Im(bw - 2, 0)
        }
        io.out_s := result
      } else {
        io.out_s.Re := (~io.in_b.Re(bw - 1)) ## io.in_b.Re(bw - 2, 0)
        io.out_s.Im := (~io.in_b.Im(bw - 1)) ## io.in_b.Im(bw - 2, 0)
      }
    } else if (sIm.abs < 0.00005) { // check if magnitude of input b is zero
      if (add_reg) {
        val result = RegInit(0.U.asTypeOf(new ComplexNum(bw,name)))
        when(io.in_en) {
          result := io.in_a
        }
        io.out_s := result
      } else {
        io.out_s := io.in_a
      }
    } else { // neither the input a or input b are zero // we must use actual FP units
      val FP_subbers = (for (i <- 0 until 2) yield {
        val fpsub = Module(new FP_subber_v2(bw)).io
        fpsub
      }).toVector
      FP_subbers(0).in_en := io.in_en
      FP_subbers(1).in_en := io.in_en
      FP_subbers(0).in_a := io.in_a.Re
      FP_subbers(0).in_b := io.in_b.Re
      FP_subbers(1).in_a := io.in_a.Im
      FP_subbers(1).in_b := io.in_b.Im
      io.out_s.Re := FP_subbers(0).out_s
      io.out_s.Im := FP_subbers(1).out_s
    }
  }
// change fp operators

  class FPComplexMult_v2(bw: Int, name:Int) extends Module {
    override def desiredName = s"FPComplexMult_v2_${name}"
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })


    
    val FP_sub = Module(new FP_subtractor_13ccs(bw,name)).io
    val FP_add = Module(new FP_adder_13ccs(bw,name)).io
    val FP_multipliers = (for (i <- 0 until 4) yield {
      val fpmult = Module(new FP_multiplier_10ccs(bw,name)).io
      fpmult
    }).toVector
    for(i <- 0 until 4){
      FP_multipliers(i).in_en := io.in_en
    }
    FP_sub.in_en := io.in_en
    FP_add.in_en := io.in_en
    FP_multipliers(0).in_a := io.in_a.Re
    FP_multipliers(0).in_b := io.in_b.Re
    FP_multipliers(1).in_a := io.in_a.Im
    FP_multipliers(1).in_b := io.in_b.Im
    FP_multipliers(2).in_a := io.in_a.Re
    FP_multipliers(2).in_b := io.in_b.Im
    FP_multipliers(3).in_a := io.in_a.Im
    FP_multipliers(3).in_b := io.in_b.Re
    FP_sub.in_a := FP_multipliers(0).out_s
    FP_sub.in_b := FP_multipliers(1).out_s
    FP_add.in_a := FP_multipliers(2).out_s
    FP_add.in_b := FP_multipliers(3).out_s
    io.out_s.Re := FP_sub.out_s
    io.out_s.Im := FP_add.out_s
  }
  class FPComplexDivider(bw: Int, name: Int) extends Module {
    override def desiredName = s"FPComplexDivider_${name}"
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw,name))
      val in_b = Input(new ComplexNum(bw,name))
      val out_s = Output(new ComplexNum(bw,name))
    })


    val FP_sub = Module(new FP_subtractor_13ccs(bw,name)).io
    val FP_add1 = Module(new FP_adder_13ccs(bw,name)).io
    val FP_add2 = Module(new FP_adder_13ccs(bw,name)).io
    val div_re = Module(new FP_divider_newfpu(bw,1,name)).io
    val div_im = Module(new FP_divider_newfpu(bw,1,name)).io
    val FP_multipliers = (for (i <- 0 until 6) yield {
      val fpmult = Module(new FP_multiplier_10ccs(bw,name)).io
      fpmult
    }).toVector
    for(i <- 0 until 6){
      FP_multipliers(i).in_en := io.in_en
    }
    FP_sub.in_en := io.in_en
    FP_add1.in_en := io.in_en
    FP_add2.in_en := io.in_en
    div_re.in_en := io.in_en
    div_im.in_en := io.in_en

    FP_multipliers(0).in_a := io.in_a.Re
    FP_multipliers(0).in_b := io.in_b.Re
    FP_multipliers(1).in_a := io.in_a.Im
    FP_multipliers(1).in_b := io.in_b.Im

    FP_multipliers(2).in_a := io.in_a.Im
    FP_multipliers(2).in_b := io.in_b.Re
    FP_multipliers(3).in_a := io.in_a.Re
    FP_multipliers(3).in_b := io.in_b.Im

    FP_multipliers(4).in_a := io.in_b.Re
    FP_multipliers(4).in_b := io.in_b.Re
    FP_multipliers(5).in_a := io.in_b.Im
    FP_multipliers(5).in_b := io.in_b.Im

    FP_sub.in_a := FP_multipliers(2).out_s
    FP_sub.in_b := FP_multipliers(3).out_s
    FP_add1.in_a := FP_multipliers(0).out_s
    FP_add1.in_b := FP_multipliers(1).out_s

    FP_add2.in_a := FP_multipliers(4).out_s
    FP_add2.in_b := FP_multipliers(5).out_s

    div_re.in_a := FP_add1.out_s
    div_re.in_b := FP_add2.out_s
    div_im.in_a := FP_sub.out_s
    div_im.in_b := FP_add2.out_s

    io.out_s.Re := div_re.out_s
    io.out_s.Im := div_im.out_s

  }





/*
  // multiplier with reducable hardware cost based on parameters
  // specifically checks to see if real or imaginary parts are powers of 2
  // it this is the case, then we can reduce the amount of multipliers used.
  // for this module, we are assuming that complex number magnitudes = 1
  class FPComplexMult_reducable_SimpleCases_v2(bw: Int, sRe: Double, sIm: Double) extends Module {
    var exponent = 0
    var mantissa = 0
    if (bw == 16) {
      exponent = 5
      mantissa = 10
    } else if (bw == 32) {
      exponent = 8
      mantissa = 23
    } else if (bw == 64) {
      exponent = 11
      mantissa = 52
    } else if (bw == 128) {
      exponent = 15
      mantissa = 112
    }
    val bias = Math.pow(2, exponent - 1) - 1
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw))
      val in_b = Input(new ComplexNum(bw))
      val out_s = Output(new ComplexNum(bw))
    })
    val FP_sub = Module(new FP_subber_v2(bw)).io
    val FP_add = Module(new FP_adder_v2(bw)).io
    FP_sub.in_en := io.in_en
    FP_add.in_en := io.in_en
    val sign = Wire(Vec(2, UInt(1.W)))
    sign(0) := io.in_a.Re(bw - 1)
    sign(1) := io.in_a.Im(bw - 1)
    val exp = Wire(Vec(2, UInt(exponent.W)))
    exp(0) := io.in_a.Re(bw - 2, mantissa)
    exp(1) := io.in_a.Im(bw - 2, mantissa)
    val frac = Wire(Vec(2, UInt(mantissa.W)))
    frac(0) := io.in_a.Re(mantissa - 1, 0)
    frac(1) := io.in_a.Im(mantissa - 1, 0)
    val new_exp1 = Wire(Vec(2, UInt(exponent.W)))
    val c1 = isReducable(sRe.abs)
    val c2 = isReducable(sIm.abs)
    val new_sign = Wire(Vec(4, UInt(1.W)))
    // computing the new signs resulting from multiplications
    new_sign(0) := sign(0) ^ io.in_b.Re(bw - 1)
    new_sign(1) := sign(0) ^ io.in_b.Im(bw - 1)
    new_sign(2) := sign(1) ^ io.in_b.Im(bw - 1)
    new_sign(3) := sign(1) ^ io.in_b.Re(bw - 1)
    when(c1._1.B) { // is the real part of the second input a power of 2 value
      when(exp(0) =/= 0.U) {
        new_exp1(0) := exp(0) - c1._2.abs.U // adjust the exponent part of the real part of the first input
      }.otherwise {
        new_exp1(0) := exp(0)
      }
      when(exp(1) =/= 0.U) {
        new_exp1(1) := exp(1) - c1._2.abs.U // adjust the exponent part of the imaginary part of the first input
      }.otherwise {
        new_exp1(1) := exp(1)
      }
    }.elsewhen(c2._1.B) { // is the imaginary part of the second input a power of 2 value
      when(exp(0) =/= 0.U) {
        new_exp1(0) := exp(0) - c2._2.abs.U // adjust exp part of real part of first input
      }.otherwise {
        new_exp1(0) := exp(0)
      }
      when(exp(1) =/= 0.U) {
        new_exp1(1) := exp(1) - c2._2.abs.U // adjust exp part of imaginary part of first input
      }.otherwise {
        new_exp1(1) := exp(1)
      }
    }.otherwise {
      new_exp1(0) := exp(0) // otherwise just keep the same exp for both real and im parts of first input
      new_exp1(1) := exp(1)
    }

    val mult_results = Wire(Vec(4, UInt(bw.W)))
    if (c1._1) { // real part of second input is a power of 2
      val regs1 = RegInit(VecInit.fill(2)(0.U(bw.W)))
      //val regs1 = Reg(Vec(2, UInt(bw.W)))
      when(io.in_en) {
        regs1(0) := new_sign(0) ## new_exp1(0) ## frac(0) // concat the sign/exp/mantissa parts into the IEEE 754 FP number
        regs1(1) := new_sign(3) ## new_exp1(1) ## frac(1) //
      }
      mult_results(0) := regs1(0) // we now have half of the multiplier results that we need
      mult_results(3) := regs1(1)
    } else { // otherwise we need to use actualy FP multipliers
      val FP_multipliers1 = (for (i <- 0 until 2) yield {
        val fpmult = Module(new FP_multiplier_v2(bw)).io
        fpmult
      }).toVector
      for(i <- 0 until 2){
        FP_multipliers1(i).in_en := io.in_en
      }
      FP_multipliers1(0).in_a := io.in_a.Re
      FP_multipliers1(0).in_b := io.in_b.Re
      FP_multipliers1(1).in_a := io.in_a.Im
      FP_multipliers1(1).in_b := io.in_b.Re
      mult_results(0) := FP_multipliers1(0).out_s
      mult_results(3) := FP_multipliers1(1).out_s
    }
    if (c2._1) { // imaginary part of second input is a power of 2.
      val regs2 = RegInit(VecInit.fill(2)(0.U(bw.W)))
      //val regs2 = Reg(Vec(2, UInt(bw.W)))
      when(io.in_en) {
        regs2(0) := new_sign(1) ## new_exp1(0) ## frac(0) // concat the sign/ exp/ mantissa parts
        regs2(1) := new_sign(2) ## new_exp1(1) ## frac(1)
      }
      mult_results(2) := regs2(0) // we know have the other half of the multiplier results that we need
      mult_results(1) := regs2(1)
    } else { // otherwise we need to use actual FP multipliers
      val FP_multipliers2 = (for (i <- 0 until 2) yield {
        val fpmult = Module(new FP_multiplier_v2(bw)).io
        fpmult
      }).toVector
      for(i <- 0 until 2){
        FP_multipliers2(i).in_en := io.in_en
      }
      FP_multipliers2(0).in_a := io.in_a.Re
      FP_multipliers2(0).in_b := io.in_b.Im
      FP_multipliers2(1).in_a := io.in_a.Im
      FP_multipliers2(1).in_b := io.in_b.Im
      mult_results(2) := FP_multipliers2(0).out_s
      mult_results(1) := FP_multipliers2(1).out_s
    }
    FP_sub.in_a := mult_results(0) // complete FP multiplier computation using adder and subtraction
    FP_sub.in_b := mult_results(1)
    FP_add.in_a := mult_results(2)
    FP_add.in_b := mult_results(3)
    io.out_s.Re := FP_sub.out_s
    io.out_s.Im := FP_add.out_s
  }

  // a more general implementation of FPComplexMult_reduceable_simplecases_v2
  // also consideres cases in which complex number magnitude != 1
  // but it can also do same function as the previous module if needed.
  // mainly used in the symmetric DFT implementation
  class FPComplexMult_reducable_forSymmetric_v2(bw: Int, sRe: Double, sIm: Double, add_reg: Boolean, special_mult: Boolean) extends Module {
    var exponent = 0
    var mantissa = 0
    if (bw == 16) {
      exponent = 5
      mantissa = 10
    } else if (bw == 32) {
      exponent = 8
      mantissa = 23
    } else if (bw == 64) {
      exponent = 11
      mantissa = 52
    } else if (bw == 128) {
      exponent = 15
      mantissa = 112
    }
    val regdpth = if (special_mult) {
      1
    } else {
      2
    }
    val bias = Math.pow(2, exponent - 1) - 1
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_a = Input(new ComplexNum(bw))
      val in_b = Input(new ComplexNum(bw))
      val out_s = Output(new ComplexNum(bw))
    })
    if (sRe.abs < 0.00005 && sIm.abs < 0.00005) { // if both real and imaginary parts of second input are 0
      if (add_reg) { // add some registers to maintain the pipeline if not done externally already
        val result = RegInit(VecInit.fill(regdpth)(0.U.asTypeOf(new ComplexNum(bw))))
        when(io.in_en) {
          for (i <- 0 until regdpth) {
            if (i == 0) {
              result(0) := 0.U.asTypeOf(new ComplexNum(bw))
            } else {
              result(i) := result(i - 1)
            }
          }
        }
        //result := cmplx_reorder.out
        io.out_s := result(regdpth - 1)
      } else {
        io.out_s := 0.U.asTypeOf(new ComplexNum(bw))
      }
    }
    else if ((sRe.abs - sIm.abs).abs < 0.000005) { // if the real part and imaginary parts have the same value. (sqrt(2)/2 for instance)
      val adder = Module(new FP_adder_v2(bw)).io
      val subber = Module(new FP_subber_v2(bw)).io
      adder.in_en := io.in_en
      subber.in_en := io.in_en
      val temp_results = Wire(Vec(2, new ComplexNum(bw)))
      if (sRe > 0 && sIm > 0) { // both im and re parts are positive
        temp_results(0).Re := io.in_a.Re
        temp_results(0).Im := io.in_a.Im
        temp_results(1).Re := io.in_a.Re
        temp_results(1).Im := io.in_a.Im
      } else if (sRe < 0 && sIm < 0) { // both im and re pats are negative
        temp_results(0).Re := (~io.in_a.Re(bw - 1)) ## io.in_a.Re(bw - 2, 0)
        temp_results(0).Im := (~io.in_a.Im(bw - 1)) ## io.in_a.Im(bw - 2, 0)
        temp_results(1).Re := (~io.in_a.Re(bw - 1)) ## io.in_a.Re(bw - 2, 0)
        temp_results(1).Im := (~io.in_a.Im(bw - 1)) ## io.in_a.Im(bw - 2, 0)
      } else if (sRe < 0) { // real part is negative, im part is positive
        temp_results(0).Re := (~io.in_a.Re(bw - 1)) ## io.in_a.Re(bw - 2, 0)
        temp_results(0).Im := io.in_a.Im
        temp_results(1).Re := io.in_a.Re
        temp_results(1).Im := (~io.in_a.Im(bw - 1)) ## io.in_a.Im(bw - 2, 0)
      } else if (sIm < 0) { // im part is negative, re part is positive
        temp_results(0).Re := io.in_a.Re
        temp_results(0).Im := (~io.in_a.Im(bw - 1)) ## io.in_a.Im(bw - 2, 0)
        temp_results(1).Re := (~io.in_a.Re(bw - 1)) ## io.in_a.Re(bw - 2, 0)
        temp_results(1).Im := io.in_a.Im
      }
      subber.in_a := temp_results(0).Re // 1 clock cycle
      subber.in_b := temp_results(0).Im
      adder.in_a := temp_results(1).Re
      adder.in_b := temp_results(1).Im
      // notice that wer are only using 2 multipliers insteadt of using 4 like we normally do for complex mults
      val multipliers = VecInit.fill(2)(Module(new FP_multiplier_v2(bw)).io)
      for(i <- 0 until 2){
        multipliers(i).in_en := io.in_en
      }
      multipliers(0).in_a := subber.out_s
      // 1 clock cycle // no need for reg layer
      multipliers(0).in_b := 0.U(1.W) ## io.in_b.Re(bw - 2, 0)
      multipliers(1).in_a := adder.out_s
      multipliers(1).in_b := 0.U(1.W) ## io.in_b.Re(bw - 2, 0)
      io.out_s.Re := multipliers(0).out_s
      io.out_s.Im := multipliers(1).out_s
    }
    else if (sRe.abs < 0.00005) { // if real part of second input is 0
      val conds = isReducable(sIm.abs)
      val cmplx_reorder = Module(new ComplexNum_AdjustOrder_v2(bw)).io
      cmplx_reorder.in_en := io.in_en
      cmplx_reorder.in := io.in_a
      cmplx_reorder.is_flip := true.B // we know that we have to multiply times imaginary part
      if (conds._1) { // if imaginary part is a power of 2
        cmplx_reorder.in_adj := conds._2.abs.U
        if (sIm < -0.00005) { // if imaginary part is negative
          cmplx_reorder.is_neg := true.B
        } else {
          cmplx_reorder.is_neg := false.B
        }
        if (add_reg) { // add reg to maintain pipeline if not already done so externally
          val result = RegInit(VecInit.fill(regdpth)(0.U.asTypeOf(new ComplexNum(bw))))
          when(io.in_en) {
            for (i <- 0 until regdpth) {
              if (i == 0) {
                result(0) := cmplx_reorder.out
              } else {
                result(i) := result(i - 1)
              }
            }
          }
          //result := cmplx_reorder.out
          io.out_s := result(regdpth - 1)
        } else {
          io.out_s := cmplx_reorder.out
        }
      } else { // if imaginary part is not a power of 2
        cmplx_reorder.in_adj := 0.U
        cmplx_reorder.is_neg := false.B
        val multipliers = for (i <- 0 until 2) yield { // use actual multipliers
          val mults = Module(new FP_multiplier_v2(bw)).io
          mults
        }
        for(i <- 0 until 2){
          multipliers(i).in_en := io.in_en
        }
        multipliers(0).in_a := cmplx_reorder.out.Re
        multipliers(0).in_b := io.in_b.Im
        multipliers(1).in_a := cmplx_reorder.out.Im
        multipliers(1).in_b := io.in_b.Im
        io.out_s.Re := multipliers(0).out_s
        io.out_s.Im := multipliers(1).out_s
      }
    } else if (sIm.abs < 0.00005) { // if the imaginary part of second input is 0
      val conds = isReducable(sRe.abs) // check if real part of second inout is a power of 2
      val cmplx_reorder = Module(new ComplexNum_AdjustOrder_v2(bw)).io
      cmplx_reorder.in_en := io.in_en
      cmplx_reorder.in := io.in_a
      cmplx_reorder.is_flip := false.B // no need to flip when we only have to multiply by a real part
      if (conds._1) { // if real part is power of 2
        cmplx_reorder.in_adj := conds._2.abs.U
        if (sRe < -0.00005) { // if real part is negative
          cmplx_reorder.is_neg := true.B
        } else {
          cmplx_reorder.is_neg := false.B
        }
        if (add_reg) { // add regs for pipeline
          val result = RegInit(VecInit.fill(regdpth)(0.U.asTypeOf(new ComplexNum(bw))))
          when(io.in_en) {
            for (i <- 0 until regdpth) {
              if (i == 0) {
                result(0) := cmplx_reorder.out
              } else {
                result(i) := result(i - 1)
              }
            }
          }
          //result := cmplx_reorder.out
          io.out_s := result(regdpth - 1)
        } else {
          io.out_s := cmplx_reorder.out
        }
      } else {
        cmplx_reorder.in_adj := 0.U
        cmplx_reorder.is_neg := false.B
        val multipliers = for (i <- 0 until 2) yield {
          val mults = Module(new FP_multiplier_v2(bw)).io
          mults
        }
        for(i <- 0 until 2){
          multipliers(i).in_en := io.in_en
        }
        multipliers(0).in_a := cmplx_reorder.out.Re
        multipliers(0).in_b := io.in_b.Re
        multipliers(1).in_a := cmplx_reorder.out.Im
        multipliers(1).in_b := io.in_b.Re
        io.out_s.Re := multipliers(0).out_s
        io.out_s.Im := multipliers(1).out_s
      }
    } else { // if we have non zero imaginary and real parts
      val FP_sub = Module(new FP_subber_v2(bw)).io
      val FP_add = Module(new FP_adder_v2(bw)).io
      FP_sub.in_en := io.in_en
      FP_add.in_en := io.in_en
      val sign = Wire(Vec(2, UInt(1.W)))
      sign(0) := io.in_a.Re(bw - 1)
      sign(1) := io.in_a.Im(bw - 1)
      val exp = Wire(Vec(2, UInt(exponent.W)))
      exp(0) := io.in_a.Re(bw - 2, mantissa)
      exp(1) := io.in_a.Im(bw - 2, mantissa)
      val frac = Wire(Vec(2, UInt(mantissa.W)))
      frac(0) := io.in_a.Re(mantissa - 1, 0)
      frac(1) := io.in_a.Im(mantissa - 1, 0)
      val new_exp1 = Wire(Vec(2, UInt(exponent.W)))
      val c1 = isReducable(sRe.abs) // check if real part is power of 2
      val c2 = isReducable(sIm.abs) // check if imaginary part is power of2
      val new_sign = Wire(Vec(4, UInt(1.W)))
      new_sign(0) := sign(0) ^ io.in_b.Re(bw - 1)
      new_sign(1) := sign(0) ^ io.in_b.Im(bw - 1)
      new_sign(2) := sign(1) ^ io.in_b.Im(bw - 1)
      new_sign(3) := sign(1) ^ io.in_b.Re(bw - 1)
      when(c1._1.B) {
        when(exp(0) =/= 0.U) {
          new_exp1(0) := exp(0) - c1._2.abs.U
        }.otherwise {
          new_exp1(0) := exp(0)
        }
        when(exp(1) =/= 0.U) {
          new_exp1(1) := exp(1) - c1._2.abs.U
        }.otherwise {
          new_exp1(1) := exp(1)
        }
      }.elsewhen(c2._1.B) {
        when(exp(0) =/= 0.U) {
          new_exp1(0) := exp(0) - c2._2.abs.U
        }.otherwise {
          new_exp1(0) := exp(0)
        }
        when(exp(1) =/= 0.U) {
          new_exp1(1) := exp(1) - c2._2.abs.U
        }.otherwise {
          new_exp1(1) := exp(1)
        }
      }.otherwise {
        new_exp1(0) := exp(0)
        new_exp1(1) := exp(1)
      }

      val mult_results = Wire(Vec(4, UInt(bw.W)))
      if (c1._1) {
        val regs1 = RegInit(VecInit.fill(2)(0.U(bw.W)))
        //val regs1 = Reg(Vec(2, UInt(bw.W)))
        when(io.in_en) {
          regs1(0) := new_sign(0) ## new_exp1(0) ## frac(0)
          regs1(1) := new_sign(3) ## new_exp1(1) ## frac(1)
        }
        mult_results(0) := regs1(0)
        mult_results(3) := regs1(1)
      } else {
        val FP_multipliers1 = (for (i <- 0 until 2) yield {
          val fpmult = Module(new FP_multiplier_v2(bw)).io
          fpmult
        }).toVector
        for(i <- 0 until 2){
          FP_multipliers1(i).in_en := io.in_en
        }
        FP_multipliers1(0).in_a := io.in_a.Re
        FP_multipliers1(0).in_b := io.in_b.Re
        FP_multipliers1(1).in_a := io.in_a.Im
        FP_multipliers1(1).in_b := io.in_b.Re
        mult_results(0) := FP_multipliers1(0).out_s
        mult_results(3) := FP_multipliers1(1).out_s
      }
      if (c2._1) {
        val regs2 = RegInit(VecInit.fill(2)(0.U(bw.W)))
        //val regs2 = Reg(Vec(2, UInt(bw.W)))
        when(io.in_en) {
          regs2(0) := new_sign(1) ## new_exp1(0) ## frac(0)
          regs2(1) := new_sign(2) ## new_exp1(1) ## frac(1)
        }
        mult_results(2) := regs2(0)
        mult_results(1) := regs2(1)
      } else {
        val FP_multipliers2 = (for (i <- 0 until 2) yield {
          val fpmult = Module(new FP_multiplier_v2(bw)).io
          fpmult
        }).toVector
        for(i <- 0 until 2){
          FP_multipliers2(i).in_en := io.in_en
        }
        FP_multipliers2(0).in_a := io.in_a.Re
        FP_multipliers2(0).in_b := io.in_b.Im
        FP_multipliers2(1).in_a := io.in_a.Im
        FP_multipliers2(1).in_b := io.in_b.Im
        mult_results(2) := FP_multipliers2(0).out_s
        mult_results(1) := FP_multipliers2(1).out_s
      }
      FP_sub.in_a := mult_results(0)
      FP_sub.in_b := mult_results(1)
      FP_add.in_a := mult_results(2)
      FP_add.in_b := mult_results(3)
      io.out_s.Re := FP_sub.out_s
      io.out_s.Im := FP_add.out_s
    }
  }
*/

/*
  // add up n complex inputs using multiple FP adders with 2 input ports each
  // kind of like a dot product after the multiplication part.
  class FPComplexMultiAdder_v2(n: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(n, new ComplexNum(bw)))
      val in_en = Input(Bool())
      val out = Output(new ComplexNum(bw))
    })
    val out_reg_save = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    if (n == 0 || n == 1) {
      when(io.in_en) {
        io.out := io.in(0)
        out_reg_save := io.in(0)
      }.otherwise{
        io.out := out_reg_save
      }
    } else {
      val FP_adders = (for (i <- 0 until n - 1) yield {
        val FP_adder = Module(new FPComplexAdder_v2(bw)).io
        FP_adder
      }).toVector
      for(i <- 0 until n -1){
        FP_adders(i).in_en := io.in_en
      }
      var count_FP_adder = 0 // keeps track of the floating point adder in use
      var inputcounter = 0 // keeps track of inputs into the floating point adders
      var tracker_sumCount = n // is keeping track of the number elements that need to be summed
      var odd = 0 // indicates if the number of elements we are trying to sum up is an odd number.
      var odd_inner = 0
      var offset = 0 //this is used to keep the counter synchronized when dealing with an odd number of summations in a layer
      var count_lastadder = n - 2 // keeps track of position of the last adder relative to the current summation layer
      var blncing_regs = 0
      var total_adders = n
      while (total_adders > 1) {
        if (total_adders % 2 == 1) {
          blncing_regs += 1
        }
        total_adders /= 2
      }
      if (blncing_regs > 0) {
        val regs = RegInit(VecInit.fill(blncing_regs)(0.U.asTypeOf(new ComplexNum(bw))))
        var reg_indx = 0
        if (tracker_sumCount % 2 != 0) { // if summing an odd number of elements
          odd = 1
          when(io.in_en) {
            regs(reg_indx) := io.in(tracker_sumCount - 1)
          }
          FP_adders(count_lastadder).in_b := regs(reg_indx)
          count_lastadder -= 1
          offset += 1
          reg_indx += 1
        }
        for (i <- 0 until tracker_sumCount / 2) { // add all the multiplication results using the first layer of FP-adders
          FP_adders(count_FP_adder).in_a := io.in(inputcounter)
          FP_adders(count_FP_adder).in_b := io.in(inputcounter + 1)
          inputcounter += 2
          count_FP_adder += 1
        }
        offset = 0
        tracker_sumCount /= 2
        inputcounter = 0
        for (i <- 0 until log2Ceil(n) - 1) { // for all the adding stages that are needed to compute the full sum
          if (tracker_sumCount == 0 || tracker_sumCount == 1) { // if we have reached the end of the adders
            FP_adders(count_FP_adder).in_a := FP_adders(count_lastadder).out_s
            if (odd_inner > 0) { // connect the rest of the unconnected modules
              for (i <- 0 until odd_inner) {
                if (count_FP_adder < n - 2 - odd && n - 2 - odd > 0) {
                  FP_adders(count_FP_adder + 1).in_a := FP_adders(count_FP_adder).out_s
                  count_FP_adder += 1
                }
              }
            }
          } else if (tracker_sumCount % 2 != 0) { // if trying to sum up an odd number of results
            odd_inner += 1
            regs(reg_indx) := FP_adders(inputcounter + tracker_sumCount - 1).out_s
            FP_adders(count_lastadder).in_b := regs(reg_indx)
            count_lastadder -= 1
            offset += 1
            reg_indx += 1
          }
          for (i <- 0 until tracker_sumCount / 2) { // add all the multiplication results using the next layer FP-adders
            FP_adders(count_FP_adder).in_a := FP_adders(inputcounter).out_s
            FP_adders(count_FP_adder).in_b := FP_adders(inputcounter + 1).out_s
            inputcounter += 2
            count_FP_adder += 1
          }
          inputcounter += offset
          offset = 0
          tracker_sumCount /= 2
        }
        if (odd == 1 && n - 2 - odd > 0) { // connect remaining module
          FP_adders(if (n - 2 > 0) n - 2 else 0).in_a := FP_adders(if (n - 2 > 0) n - 2 - 1 else 0).out_s
        }
        io.out := FP_adders(n - 2).out_s
      } else {
        var reg_indx = 0
        if (tracker_sumCount % 2 != 0) { // if summing an odd number of elements
          odd = 1
          FP_adders(count_lastadder).in_b := io.in(tracker_sumCount - 1)
          count_lastadder -= 1
          offset += 1
        }
        for (i <- 0 until tracker_sumCount / 2) { // add all the multiplication results using the first layer of FP-adders
          FP_adders(count_FP_adder).in_a := io.in(inputcounter)
          FP_adders(count_FP_adder).in_b := io.in(inputcounter + 1)
          inputcounter += 2
          count_FP_adder += 1
        }
        offset = 0
        tracker_sumCount /= 2
        inputcounter = 0
        for (i <- 0 until log2Ceil(n) - 1) { // for all the adding stages that are needed to compute the full sum
          if (tracker_sumCount == 0 || tracker_sumCount == 1) { // if we have reached the end of the adders
            FP_adders(count_FP_adder).in_a := FP_adders(count_lastadder).out_s
            if (odd_inner > 0) { // connect the rest of the unconnected modules
              for (i <- 0 until odd_inner) {
                if (count_FP_adder < n - 2 - odd && n - 2 - odd > 0) {
                  FP_adders(count_FP_adder + 1).in_a := FP_adders(count_FP_adder).out_s
                  count_FP_adder += 1
                }
              }
            }
          } else if (tracker_sumCount % 2 != 0) { // if trying to sum up an odd number of results
            odd_inner += 1
            FP_adders(count_lastadder).in_b := FP_adders(inputcounter + tracker_sumCount - 1).out_s
            count_lastadder -= 1
            offset += 1
          }
          for (i <- 0 until tracker_sumCount / 2) { // add all the multiplication results using the next layer FP-adders
            FP_adders(count_FP_adder).in_a := FP_adders(inputcounter).out_s
            FP_adders(count_FP_adder).in_b := FP_adders(inputcounter + 1).out_s
            inputcounter += 2
            count_FP_adder += 1
          }
          inputcounter += offset
          offset = 0
          tracker_sumCount /= 2
        }
        if (odd == 1 && n - 2 - odd > 0) { // connect remaining module
          FP_adders(if (n - 2 > 0) n - 2 else 0).in_a := FP_adders(if (n - 2 > 0) n - 2 - 1 else 0).out_s
        }
        io.out := FP_adders(n - 2).out_s
      }
    }
  }
*/
/*
  // used in the case that a full FP multiplier is not entirely needed to achieve the result.
  // perhaps it is the case that a complex number is being multiplied by power of 2 or 1, or a negative value
  // this module takes all of that into account and computes the outcome that one would get with the
  // fp multiplier using a much lower resource cost module.
  class ComplexNum_AdjustOrder_v2(bw: Int) extends Module {
    var exponent = 0
    var mantissa = 0
    if (bw == 16) {
      exponent = 5
      mantissa = 10
    } else if (bw == 32) {
      exponent = 8
      mantissa = 23
    } else if (bw == 64) {
      exponent = 11
      mantissa = 52
    } else if (bw == 128) {
      exponent = 15
      mantissa = 112
    }
    val bias = Math.pow(2, exponent - 1) - 1
    val io = IO(new Bundle() {
      val in = Input(new ComplexNum(bw))
      val in_en = Input(Bool())
      val in_adj = Input(UInt(exponent.W)) // adjust the exponent part of the complex input
      val is_neg = Input(Bool()) // identify if we have to multiply the input times a negative 1
      val is_flip = Input(Bool()) // identify whether we are multiplying the input by a purely real num or imaginary number
      val out = Output(new ComplexNum(bw))
    })
    val sign = Wire(Vec(2, UInt(1.W)))
    sign(0) := io.in.Re(bw - 1)
    sign(1) := io.in.Im(bw - 1)
    val exp = Wire(Vec(2, UInt(exponent.W)))
    exp(0) := io.in.Re(bw - 2, mantissa)
    exp(1) := io.in.Im(bw - 2, mantissa)
    val frac = Wire(Vec(2, UInt(mantissa.W)))
    frac(0) := io.in.Re(mantissa - 1, 0)
    frac(1) := io.in.Im(mantissa - 1, 0)
    val new_sign = Wire(Vec(2, UInt(1.W)))
    when(io.is_neg) { // if multiplying by negative, invert the sign of the input
      new_sign(0) := ~sign(0)
      new_sign(1) := ~sign(1)
    }.otherwise {
      new_sign(0) := sign(0)
      new_sign(1) := sign(1)
    }
    val new_exp = Wire(Vec(2, UInt(exponent.W)))
    when(exp(0) =/= 0.U) {
      new_exp(0) := exp(0) - io.in_adj // adjust the exponent of the input
    }.otherwise {
      new_exp(0) := exp(0)
    }
    when(exp(1) =/= 0.U) {
      new_exp(1) := exp(1) - io.in_adj // adjust the exponent of the input
    }.otherwise {
      new_exp(1) := exp(1)
    }

    val out_reg_save = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    when(io.is_flip) { // we flip the real part to the imaginary part and imaginary to real part
      when(io.in_en) {
        io.out.Re := (~new_sign(1)) ## new_exp(1) ## frac(1)
        io.out.Im := (new_sign(0)) ## new_exp(0) ## frac(0)
        out_reg_save.Re := (~new_sign(1)) ## new_exp(1) ## frac(1)
        out_reg_save.Im := (new_sign(0)) ## new_exp(0) ## frac(0)
      }.otherwise{
        io.out.Re := out_reg_save.Re
        io.out.Im := out_reg_save.Im
      }
    }.otherwise { // otherwise leave the order the same.
      when(io.in_en) {
        io.out.Re := (new_sign(0)) ## new_exp(0) ## frac(0)
        io.out.Im := (new_sign(1)) ## new_exp(1) ## frac(1)
        out_reg_save.Re := (new_sign(0)) ## new_exp(0) ## frac(0)
        out_reg_save.Im := (new_sign(1)) ## new_exp(1) ## frac(1)
      }.otherwise{
        io.out.Re := out_reg_save.Re
        io.out.Im := out_reg_save.Im
      }
    }
  }
*/
  class FPReg(depth: Int,bw: Int, name:Int) extends Module{
    override def desiredName = s"FPReg_${name}"
    val io = IO(new Bundle() {
      val in = Input(new ComplexNum(bw,name))
      val out = Output(new ComplexNum(bw,name))
    })

   // val reg = RegInit(VecInit.fill(depth)(0.U(bw.W)))
    //    val reg = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    val reg = RegInit(VecInit.fill(depth)(0.U.asTypeOf(new ComplexNum(bw,name))))
      reg(0) := io.in
      for(i <- 1 until depth){
        reg(i) := reg(i-1)
      }
    io.out := reg(depth - 1)
  }



/*

  class sqrtbb_viv2(bw:Int)extends BlackBox{
        val io = IO {
            new Bundle() {
                val aclk = Input(Clock())
                val s_axis_a_tvalid = Input(Bool())
                val s_axis_a_tdata = Input(UInt((bw).W))
                val m_axis_result_tdata = Output((UInt((bw).W)))
                val m_axis_result_tvalid = Output(Bool())
                val s_axis_a_tready = Output(Bool())
            }
        }
    }

    class divider_viv2(bw:Int)extends BlackBox{
        val io = IO {
            new Bundle() {
                val aclk = Input(Clock())
                val s_axis_a_tvalid = Input(Bool())
                val s_axis_a_tdata = Input(UInt((bw).W))
                val s_axis_a_tready = Output(Bool())

                val m_axis_result_tdata = Output((UInt((bw).W)))
                val m_axis_result_tvalid = Output(Bool())


                val s_axis_b_tvalid = Input(Bool())
                val s_axis_b_tdata = Input(UInt((bw).W))
                val s_axis_b_tready = Output(Bool())
            }
        }
    }

    */

  class hqr5_complex(bw: Int,name:Int) extends Module{
    override def desiredName = s"hqr5_complex_${name}"
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      val in_a = Input(new ComplexNum(bw,name))//x1
      val in_b = Input(new ComplexNum(bw,name))//d2
      val x1_old = Output(UInt((bw).W))
      val out_s = Output(new ComplexNum(bw,name))
    })
    
    //val sqrt = Module(new sqrtbb_viv2(bw)).io

    //val sqrt = Module(new sqrtbb_viv2(bw)).io
    val sqrt = Module(new FP_square_root_newfpu(bw,3,name)).io
    val multiplier1 = Module(new FPComplexMult_v2(bw,name)).io
    val multiplier2 = Module(new FPComplexMult_v2(bw,name)).io
    //val multiplier3 = Module(new FP_multiplier_10ccs(bw)).io
    //val multiplier4 = Module(new FP_multiplier_10ccs(bw)).io
    //val recip1 = Module(new FP_reciprocal_newfpu(bw,1)).io
    val div = Module(new FP_divider_newfpu(bw,1,name)).io
    val div2= Module(new FP_divider_newfpu(bw,1,name)).io

    val adder = Module(new FPComplexAdder_v2(bw,name)).io
    val cd1 = Reg(UInt(bw.W))
    val cd2 = Reg(UInt(bw.W))
    val ca = Reg(UInt(bw.W))
    val s = Reg(new ComplexNum(bw,name))
    
   // val recipout1 = Reg(UInt(bw.W))
    val conjugate = Wire(new ComplexNum(bw,name))
    conjugate.Re := io.in_a.Re
    conjugate.Im := io.in_a.Im^("h8000_0000".U)//xor togle im

    multiplier1.in_en := 1.B
    multiplier1.in_a := conjugate
    multiplier1.in_b := io.in_a
    cd1 := multiplier1.out_s.Re
    io.x1_old := cd1

/*//////////////////////////////////////////////vivado
    sqrt.aclk := clock
    sqrt.s_axis_a_tvalid := true.B
    sqrt.s_axis_a_tdata := cd1
    cd2 := sqrt.m_axis_result_tdata
*////////////////////////////////////////////////
    
    sqrt.in_en := 1.B
    sqrt.in_a := cd1
    cd2 := sqrt.out_s

   /* d2_update := hqr3.m_axis_result_tdata
    sqrt.in_en := 1.B
    sqrt.in_a := cd1
    cd2 := sqrt.out_s*/

    /*recip1.in_en := 1.B
    recip1.in_a := cd2
    recipout1 := recip1.out_s

    multiplier3.in_en := 1.B
    multiplier3.in_a := io.in_a.Re
    multiplier3.in_b := recipout1*/


   /*////////////////VIVADO//////////////
   div.aclk := clock
   div.s_axis_b_tdata := cd2
   div.s_axis_b_tvalid := true.B
   div.s_axis_a_tvalid := true.B
   div.s_axis_a_tdata := io.in_a.Re


   div2.aclk := clock
   div2.s_axis_b_tdata := cd2
   div2.s_axis_b_tvalid := true.B
   div2.s_axis_a_tvalid := true.B
   div2.s_axis_a_tdata := io.in_a.Im
*//////////////////////////////////////

    div.in_en := 1.B
    div.in_a := io.in_a.Re
    div.in_b := cd2



    div2.in_en := 1.B
    div2.in_a := io.in_a.Im
    div2.in_b := cd2


    /*multiplier4.in_en := 1.B
    multiplier4.in_a := io.in_a.Im
    multiplier4.in_b := recipout1*/

    if(io.in_a.Re == 0.U && io.in_a.Im == 0.U){
      s.Re := "h_bf80_0000".U
      s.Im := "h_0000_0000".U
    }else{
      /*
      s.Re := multiplier3.out_s
      s.Im := multiplier4.out_s
      */

/*////////////////VIVADO////////////// 
      s.Re := div.m_axis_result_tdata
      s.Im := div2.m_axis_result_tdata
*//////////////////////////////////////

      s.Re := div.out_s
      s.Im := div2.out_s
    }

    multiplier2.in_en := 1.B
    multiplier2.in_a := s
    multiplier2.in_b := io.in_b

    adder.in_en := 1.B
    adder.in_a := io.in_a
    adder.in_b := multiplier2.out_s

    io.out_s := adder.out_s
    
  }




  class FP_DDOT_dp_complex(bw: Int, n: Int, name:Int) extends Module {
    override def desiredName = s"FP_DDOT_dp_complex_${name}"
    val io = IO(new Bundle{
      val in_a = Input(Vec(n, new ComplexNum(bw,name)))
      val in_b = Input(Vec(n, new ComplexNum(bw,name)))
      val out_s = Output(new ComplexNum(bw,name))
    })
    var temp_n = n
    val add_per_layer = mutable.ArrayBuffer[Int]()
    val regs_per_layer = mutable.ArrayBuffer[Int]()
    while(temp_n>1){
      if(temp_n % 2 == 1){
        add_per_layer += temp_n/2
        temp_n /= 2
        temp_n += 1
        regs_per_layer += 1
      }else{
        add_per_layer += temp_n/2
        temp_n /= 2
        regs_per_layer += 0
      }
    }
    val multipliers = Vector.fill(n)(Module(new FPComplexMult_v2(bw,name)).io)
    multipliers.zipWithIndex.map(x=>x._1.in_a := io.in_a(x._2))
    multipliers.zipWithIndex.map(x=>x._1.in_b := io.in_b(x._2))
    multipliers.map(_.in_en := true.B)
    if(add_per_layer.nonEmpty) {
      val regs_and_adds = for (i <- 0 until add_per_layer.length) yield {
        val adds = for (j <- 0 until add_per_layer(i)) yield {
          Module(new FPComplexAdder_v2(bw,name)).io
        }
        adds.map(_.in_en := true.B)
        val regs = for (j <- 0 until regs_per_layer(i)) yield {
          //RegInit(0.U.asTypeOf(new ComplexNum(bw)))
          Module(new FPReg(13, bw,name)).io
        }
        (adds, regs)
      }
      //      regs_and_adds(0)._1.map(_.in_en := true.B)
      for (i <- 0 until regs_and_adds.length) {
        for (j <- 0 until add_per_layer(i) * 2 by 2) {
          val temp = if (i == 0) {
            multipliers(j).out_s
          } else {
            regs_and_adds(i - 1)._1(j).out_s
          }
          val temp2 = if (i == 0) {
            multipliers(j + 1).out_s
          } else {
            if (j / 2 == add_per_layer(i) - 1) {
              if (regs_per_layer(i - 1) == 1 && add_per_layer(i - 1) % 2 == 1) {
                regs_and_adds(i - 1)._2(0).out
              } else {
                regs_and_adds(i - 1)._1(j + 1).out_s
              }
            } else {
              regs_and_adds(i - 1)._1(j + 1).out_s
            }
          }
          //          regs_and_adds(i)._1(j / 2).in_en := io.in_en
          regs_and_adds(i)._1(j / 2).in_a := temp
          regs_and_adds(i)._1(j / 2).in_b := temp2
        }
        for (j <- 0 until regs_per_layer(i)) {
          val temp = if (i == 0) {
            multipliers(add_per_layer(i) * 2).out_s
          } else {
            if (regs_per_layer(i - 1) == 1) {
              regs_and_adds(i - 1)._2(0).out
            } else {
              regs_and_adds(i - 1)._1(add_per_layer(i) * 2).out_s
            }
          }
          //          regs_and_adds(i)._2(j).in_en := true.B
          regs_and_adds(i)._2(j).in := temp
        }
      }
      //      println(s"addper layer: ${add_per_layer}")
      io.out_s := RegNext(regs_and_adds(add_per_layer.length - 1)._1(0).out_s)
    }else{
      io.out_s :=  RegNext(multipliers(0).out_s)
    }
  }

  class axpy_dp_complex(bw:Int,level:Int, name:Int)extends Module {
    override def desiredName = s"axpy_dp_complex_${name}"
    val io = IO {
      new Bundle() {
        val in_a = Input(new ComplexNum(bw,name))
        val in_b = Input(Vec((level), new ComplexNum(bw,name)))
        val in_c = Input(Vec((level), new ComplexNum(bw,name)))
        val out_s = Output(Vec((level), new ComplexNum(bw,name)))
      }
    }

    val multiply_layer = for(i <- 0 until level)yield{
      val multiply = Module(new FPComplexMult_v2(bw,name)).io
      multiply.in_en := true.B
      multiply
    }
    val adder_layer = for (i <- 0 until level) yield {
      val adder = Module(new FPComplexAdder_v2(bw ,name)).io
      adder.in_en := true.B
      adder
    }

    val reg_array_h = for (j <- 0 until level) yield {
          //RegInit(0.U.asTypeOf(new ComplexNum(bw)))
          Module(new FPReg(23, bw,name)).io
        }
   
    for(i <- 0 until level){
      multiply_layer(i).in_a := io.in_a
      multiply_layer(i).in_b := io.in_b(i)
      reg_array_h(i).in := io.in_c(i)
      adder_layer(i).in_a :=  multiply_layer(i).out_s
      adder_layer(i).in_b := reg_array_h(i).out
      io.out_s(i) := adder_layer(i).out_s
    }
  }
    def main(args: Array[String]) : Unit = {
    val sw2 = new PrintWriter("cplx_div.v")
    //val sw2 = new PrintWriter("DDOT_COMPLEX.v")
    //val sw2 = new PrintWriter("AXPY_COMPLEX.v")
    //sw2.println(getVerilogString(new FP_DDOT_dp_complex(32, 4)))
    //sw2.println(getVerilogString(new FPComplexDivider(32)))
    //sw2.println(getVerilogString(new axpy_dp_complex(32,4)))

    sw2.close()
  }

}
