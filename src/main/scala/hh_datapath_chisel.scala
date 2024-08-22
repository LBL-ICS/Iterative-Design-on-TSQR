

 /*//*****************************************
 * 10/19/2023
 * Author: Blair Reasoner
 * Release version: 1
 * 
 * NOTE: Main module to be converted to 
 * RawModule in the future
 * **************************************/
*/



package tsqr_hh_datapath
import Binary_Modules.BinaryDesigns._
import FP_Modules.FloatingPointDesigns._
import chisel3._
import chisel3.util._
import Chisel.{log2Ceil, log2Floor}
import chiseltest.RawTester.test
import chisel3.tester._
import chisel3.{RawModule, withClockAndReset}
import ComplexModules.FPComplex._

import java.io.PrintWriter
import scala.collection.mutable

object hh_datapath_chisel{

  def main(args: Array[String]) : Unit = {
    val sw2 = new PrintWriter("hh_datapath_16_chisel_test.v")
    sw2.println(getVerilogString(new hh_datapath(12, 64, 32, 4, 256)))
    sw2.close()
  }
//main datapath design module

  class hh_datapath(name:Int, bw:Int, streaming_width:Int, ddot_st:Int, total_st:Int ) extends RawModule{
        val clk = IO(Input(Clock()))
        val rst = IO(Input(Bool()))
        //val hh_cnt = IO(Input(UInt((16).W)))

        
        val d1_rdy = IO(Input(Bool()))
        val d1_vld = IO(Input(Bool()))
        val x1_old_vld = IO(Input(Bool())) 
        val vk1_vld = IO(Input(Bool()))
        val tk_vld = IO(Input(Bool()))

        val xk_axpy_rotate = IO(Input(Bool()))
        val xk_d4_rotate = IO(Input(Bool()))
        val yjp_vld = IO(Input(Bool()))

        val yj_sft = IO(Input(Bool()))

        val hh_din = IO(Input(UInt((streaming_width*bw).W)))
        val hh_dout = IO(Output(UInt((streaming_width*bw).W)))


        val cnt_en_1 = IO(Input(Vec(log2Ceil(total_st/ddot_st), Bool())))
        val reg_en_1 = IO(Input(Vec(log2Ceil(total_st/ddot_st), Bool())))
        val cnt_en_2 = IO(Input(Vec(log2Ceil(total_st/streaming_width), Bool())))
        val reg_en_2 = IO(Input(Vec(log2Ceil(total_st/streaming_width), Bool())))



  
  withClockAndReset (clk,rst){

    def counter(max: UInt, enable: Bool) = {
      val x = RegInit(0.asUInt(max.getWidth.W))
          x := Mux(~enable,0.U,Mux(x === max, 0.U, x + 1.U))
          x
      }

    def pulse(n: UInt, enable: Bool) = counter((n),enable) === (n)

    var ddot1_it = log2Ceil(total_st/ddot_st)
    var ddot2_it = log2Ceil(total_st/streaming_width)


    val x1 = Wire(UInt(bw.W))
    val tk = RegInit(0.U((bw/2).W))
    val x1_old = RegInit(0.U((bw/2).W))
    val d1 = RegInit(0.U((bw/2).W))


    


    
    val ddot1 = Module(new FP_DDOT_dp(ddot_st*2,bw/2,name)).io
    for(i <- 0 until ddot_st ){
        ddot1.in_a(i*2) := (hh_din(ddot_st*bw-(i*bw)-1,(ddot_st*bw-(bw*(i+1)))))(bw-1,bw/2)
        ddot1.in_b(i*2) := (hh_din(ddot_st*bw-(i*bw)-1,(ddot_st*bw-(bw*(i+1)))))(bw-1,bw/2)
        ddot1.in_a(i*2+1) := (hh_din(ddot_st*bw-(i*bw)-1,(ddot_st*bw-(bw*(i+1)))))((bw/2)-1,0)
        ddot1.in_b(i*2+1) := (hh_din(ddot_st*bw-(i*bw)-1,(ddot_st*bw-(bw*(i+1)))))((bw/2)-1,0)
    }

    val acc1_input_pulse_layer = for(i <- 0 until ddot1_it)yield{
    val acc1_pulse = pulse((((1<<(i+1))-1).U), cnt_en_1(i))
    acc1_pulse
    }

    val acc1_reg_pulse_layer = for(i <- 0 until ddot1_it)yield{
    val acc1_pulse = pulse((((1<<(i+2))-1).U), reg_en_1(i))
    acc1_pulse
    }


    val acc1_add_layer = for(i <- 0 until ddot1_it)yield{
    val add = Module(new FP_adder_13ccs(bw/2,name)).io
    add.in_en := true.B
    add
    }

    val acc1_reg_layer = for(i <- 0 until ddot1_it)yield{
    val sft_reg = RegInit(0.U((bw/2).W))
    sft_reg
    }

    when(rst){
        acc1_add_layer(0).in_a := 0.U
        acc1_add_layer(0).in_b := 0.U
    }.elsewhen(acc1_input_pulse_layer(0)){
        acc1_add_layer(0).in_a := RegNext(ddot1.out_s)
        acc1_add_layer(0).in_b := ddot1.out_s
    }.otherwise{
        acc1_add_layer(0).in_a := 0.U
        acc1_add_layer(0).in_b := 0.U
    }

    for(i <- 0 until ddot1_it-1){
        when(rst){
            acc1_reg_layer(i) := 0.U
        }.elsewhen(acc1_reg_pulse_layer(i)){
            acc1_reg_layer(i) := acc1_add_layer(i).out_s
        }.otherwise{
            acc1_reg_layer(i) := acc1_reg_layer(i)
        }
    }

    when(rst || ~reg_en_1(ddot1_it-1)){
              acc1_reg_layer(ddot1_it-1) := 0.U
        }.elsewhen(reg_en_1(ddot1_it-1)  && ShiftRegister(acc1_input_pulse_layer(ddot1_it-1),13,true.B)){
              acc1_reg_layer(ddot1_it-1) := acc1_add_layer(ddot1_it-1).out_s
        }.otherwise{
              acc1_reg_layer(ddot1_it-1) := acc1_reg_layer(ddot1_it-1)
        }

    for(i <- 1 until ddot1_it){
        when(rst){
          acc1_add_layer(i).in_a := 0.U
          acc1_add_layer(i).in_b := 0.U

        }.elsewhen(acc1_input_pulse_layer(i)){
          acc1_add_layer(i).in_a := acc1_reg_layer(i-1)
          acc1_add_layer(i).in_b := acc1_add_layer(i-1).out_s
        }.otherwise{
          acc1_add_layer(i).in_a := 0.U
          acc1_add_layer(i).in_b := 0.U

        }
    }


    val hqr3 = Module(new FP_square_root_newfpu(bw/2,3,name)).io//stays the same for complex
    hqr3.in_en := true.B
    hqr3.in_a := acc1_reg_layer(ddot1_it-1)//d1
    //d2 := hqr3.out_s

    

    val hqr5= Module(new hqr5_complex(bw/2,name)).io
    hqr5.in_a.Re := x1(bw-1,bw/2)
    hqr5.in_a.Im := x1((bw/2)-1,0)
    hqr5.in_b.Re := hqr3.out_s//d2
    hqr5.in_b.Im := 0.U



    val xk_reg = RegInit(0.U(((total_st)*bw).W))
    when(d1_rdy){
      xk_reg := Cat(xk_reg(total_st*bw-(bw*ddot_st)-1,0), hh_din(total_st*bw-1,(total_st-ddot_st)*bw))
    }.elsewhen(vk1_vld){
      xk_reg := Cat(Cat(hqr5.out_s.Re,hqr5.out_s.Im),xk_reg(total_st*bw-(bw)-1,0))
    }.elsewhen(xk_d4_rotate){
      xk_reg := Cat(xk_reg((total_st-streaming_width)*bw-1,0),xk_reg(total_st*bw-1,(total_st-streaming_width)*bw))
    }.otherwise{
      xk_reg := xk_reg
    }

    x1:= xk_reg(total_st*bw-1,total_st*bw-bw)

    val xk_reg_for_axpy = RegInit(0.U(((total_st)*bw).W))
    when(RegNext(vk1_vld)){
      xk_reg_for_axpy := xk_reg
    }.elsewhen(xk_axpy_rotate){
      xk_reg_for_axpy := Cat(xk_reg_for_axpy((total_st-streaming_width)*bw-1,0),xk_reg_for_axpy(total_st*bw-1,(total_st-streaming_width)*bw))
    }.otherwise{
      xk_reg_for_axpy := xk_reg_for_axpy
    }

    
    //vk1_update := Cat(hqr5.out_s.Re,hqr5.out_s.Im)


    when(x1_old_vld){
      x1_old := hqr5.x1_old
    }.otherwise{
      x1_old := x1_old
    }

    when(d1_vld){
      d1 := acc1_reg_layer(ddot1_it-1)
    }.otherwise{
      d1 := d1
    }
  

    val d3_sub = Module(new FP_subtractor_13ccs(bw/2,name)).io
    d3_sub.in_en := true.B
    d3_sub.in_a := d1
    d3_sub.in_b := x1_old


    val x1_mult_real = Module(new FP_multiplier_10ccs(bw/2,name)).io
    x1_mult_real.in_en := true.B
    x1_mult_real.in_a := x1(bw-1,bw/2) 
    x1_mult_real.in_b := x1(bw-1,bw/2)

    val x1_mult_im = Module(new FP_multiplier_10ccs(bw/2,name)).io
    x1_mult_im.in_en := true.B
    x1_mult_im.in_a := x1(bw/2-1,0)
    x1_mult_im.in_b := x1(bw/2-1,0)   

    val x1_add = Module(new FP_adder_13ccs(bw/2,name)).io
    x1_add.in_en := true.B
    x1_add.in_a := x1_mult_real.out_s
    x1_add.in_b := x1_mult_im.out_s
    
    val d3_add = Module(new FP_adder_13ccs(bw/2,name)).io
    d3_add.in_en := true.B
    d3_add.in_a := d3_sub.out_s
    d3_add.in_b := x1_add.out_s


    





    val hqr7= Module(new hqr7(bw/2,name)).io
    hqr7.in_a := d3_add.out_s//d3
    //tk_update := hqr7.out_s

    when(tk_vld){
      tk := hqr7.out_s
    }.otherwise{
      tk := tk
    }

    
    val ddot2 = Module(new FP_DDOT_dp_complex(bw/2,streaming_width,name)).io
    for(i <- 0 until streaming_width ){ 
       ddot2.in_a(i).Re := (xk_reg(total_st*bw-1-(i*bw),total_st*bw-(i*bw+bw)))(bw-1,bw/2)
       ddot2.in_b(i).Re := (hh_din(streaming_width*bw-1-(i*bw),streaming_width*bw-(i*bw+bw)))(bw-1,bw/2)
       ddot2.in_a(i).Im := ((xk_reg(total_st*bw-1-(i*bw),total_st*bw-(i*bw+bw)))(bw/2-1,0))^("h8000_0000".U)
       ddot2.in_b(i).Im := (hh_din(streaming_width*bw-1-(i*bw),streaming_width*bw-(i*bw+bw)))(bw/2-1,0)
     }

    val yj_reg_vec = Reg(Vec(streaming_width,UInt(((log2Ceil(streaming_width)*13+24+129+22)*2*bw).W)))//adjust
    val yj0 = Reg(UInt((streaming_width*bw).W))


    when(rst){
      yj0 := 0.U
      for(i <- 0 until streaming_width){
        yj_reg_vec(i):= 0.U}
   }.elsewhen(yj_sft){
      yj0 := yj_reg_vec(streaming_width-1)(streaming_width*bw-1,0)
      yj_reg_vec(0) := Cat(hh_din,yj_reg_vec(0)(((log2Ceil(streaming_width)*13+24+129+22)*2*bw -1),streaming_width*bw))//adjust
      for(i <- 1 until streaming_width){
      yj_reg_vec(i):= Cat(yj_reg_vec(i-1)(streaming_width*bw-1,0),yj_reg_vec(i)(((log2Ceil(streaming_width)*13+24+129+22)*2*bw-1),streaming_width*bw))
    }}


     val acc2_input_pulse_layer = for(i <- 0 until ddot2_it)yield{
            val acc2_pulse = pulse((((1<<(i+1))-1).U), cnt_en_2(i))
            acc2_pulse
            }

            val acc2_reg_pulse_layer = for(i <- 0 until ddot2_it)yield{
            val acc2_pulse = pulse((((1<<(i+2))-1).U), reg_en_2(i))
            acc2_pulse
            }


            val acc2_add_layer = for(i <- 0 until ddot2_it)yield{
            val add = Module(new FPComplexAdder_v2(bw/2,name)).io
            add.in_en := true.B
            add
            }

            val acc2_reg_layer = for(i <- 0 until ddot2_it)yield{
            val sft_reg = RegInit(0.U((bw).W))
            sft_reg
            }

            when(rst){
                acc2_add_layer(0).in_a.Re := 0.U
                acc2_add_layer(0).in_b.Re := 0.U
                acc2_add_layer(0).in_a.Im := 0.U
                acc2_add_layer(0).in_b.Im := 0.U
            }.elsewhen(acc2_input_pulse_layer(0)){
                acc2_add_layer(0).in_a.Re := RegNext(ddot2.out_s.Re)
                acc2_add_layer(0).in_b.Re := ddot2.out_s.Re
                acc2_add_layer(0).in_a.Im := RegNext(ddot2.out_s.Im)
                acc2_add_layer(0).in_b.Im := ddot2.out_s.Im
            }.otherwise{
                acc2_add_layer(0).in_a.Re := 0.U
                acc2_add_layer(0).in_b.Re := 0.U
                acc2_add_layer(0).in_a.Im := 0.U
                acc2_add_layer(0).in_b.Im := 0.U

            }

            for(i <- 0 until ddot2_it-1){
                when(rst){
                    acc2_reg_layer(i) := 0.U
                }.elsewhen(acc2_reg_pulse_layer(i)){
                    acc2_reg_layer(i) := Cat(acc2_add_layer(i).out_s.Re,acc2_add_layer(i).out_s.Im)
                }.otherwise{
                    acc2_reg_layer(i) := acc2_reg_layer(i)
                }
            }

            when(rst || ~reg_en_2(ddot2_it-1)){
                     acc2_reg_layer(ddot2_it-1) := 0.U
                }.elsewhen(reg_en_2(ddot2_it-1)  && ShiftRegister(acc2_input_pulse_layer(ddot2_it-1),13,true.B)){
                     acc2_reg_layer(ddot2_it-1) := Cat(acc2_add_layer(ddot2_it-1).out_s.Re,acc2_add_layer(ddot2_it-1).out_s.Im)
                }.otherwise{
                     acc2_reg_layer(ddot2_it-1) := acc2_reg_layer(ddot2_it-1)
                }


            for(i <- 1 until ddot2_it){
                when(rst){
                  acc2_add_layer(i).in_a.Re := 0.U
                  acc2_add_layer(i).in_b.Re := 0.U
                  acc2_add_layer(i).in_a.Im := 0.U
                  acc2_add_layer(i).in_b.Im := 0.U
                }.elsewhen(acc2_input_pulse_layer(i)){
                  acc2_add_layer(i).in_a.Re := acc2_reg_layer(i-1)(bw-1,bw/2)
                  acc2_add_layer(i).in_b.Re := acc2_add_layer(i-1).out_s.Re
                  acc2_add_layer(i).in_a.Im := acc2_reg_layer(i-1)(bw/2-1,0)
                  acc2_add_layer(i).in_b.Im := acc2_add_layer(i-1).out_s.Im
                }.otherwise{
                  acc2_add_layer(i).in_a.Re := 0.U
                  acc2_add_layer(i).in_b.Re := 0.U
                  acc2_add_layer(i).in_a.Im := 0.U
                  acc2_add_layer(i).in_b.Im := 0.U
                }
            }


            val hqr10= Module(new FPComplexMult_v2(bw/2,name)).io//changes made for complex
            hqr10.in_en := true.B
            hqr10.in_a.Re := acc2_reg_layer(ddot2_it-1)(bw-1,bw/2)//d4
            hqr10.in_a.Im := acc2_reg_layer(ddot2_it-1)((bw/2)-1,0)//d4
            hqr10.in_b.Re := tk
            hqr10.in_b.Im := 0.U
            //d5 := Cat(hqr10.out_s.Re,hqr10.out_s.Im)


            val axpy= Module(new axpy_dp_complex(bw/2,streaming_width,name)).io
            axpy.in_a.Re := hqr10.out_s.Re
            axpy.in_a.Im := hqr10.out_s.Im
            for(i <- 0 until streaming_width ){ 
               axpy.in_b(i).Re := xk_reg_for_axpy(streaming_width*bw-(i*bw)-1,(streaming_width*bw-(bw*(i+1))))(bw-1,bw/2)
               axpy.in_b(i).Im := xk_reg_for_axpy(streaming_width*bw-(i*bw)-1,(streaming_width*bw-(bw*(i+1))))((bw/2)-1,0)
            
               axpy.in_c(i).Re := yj0(streaming_width*bw-(i*bw)-1,(streaming_width*bw-(bw*(i+1))))(bw-1,bw/2)
               axpy.in_c(i).Im := yj0(streaming_width*bw-(i*bw)-1,(streaming_width*bw-(bw*(i+1))))((bw/2)-1,0)
            }


  val myAxpyVec = Reg(Vec(streaming_width, UInt(width = bw.W)))
   when(rst){
    for(i <- 0 until streaming_width){
      myAxpyVec(streaming_width-i-1) := 0.U}
    }.otherwise{
      for(i <- 0 until streaming_width){
        myAxpyVec(streaming_width-i-1) := Cat(axpy.out_s(i).Re,axpy.out_s(i).Im)
      }
    }
    when(yjp_vld){
      hh_dout := myAxpyVec.asUInt
    }.otherwise{
      hh_dout := 0.U
    }
    



  }
}
}
