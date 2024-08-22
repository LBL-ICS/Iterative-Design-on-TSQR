/*************************************
 * 10-19-2023
 * Author: Blair Reasoner
 * Release version: 1
 * **********************************/

package tsqr_hh_core
import tsqr_hh_datapath.hh_datapath_chisel._
import Binary_Modules.BinaryDesigns._
import FP_Modules.FloatingPointDesigns._
import chisel3._
import chisel3.util._
import Chisel.{log2Ceil, log2Floor}
import chiseltest.RawTester.test
import chisel3.tester._
import java.io.PrintWriter
import scala.collection.mutable
import chisel3.aop.Select.When
import firrtl.bitWidth
import javax.smartcardio.Card

object Main{
  def main(args: Array[String]) : Unit = {
    val sw2 = new PrintWriter("hh_core_16_chisel_test_v2.v")
    sw2.println(getVerilogString(new hh_core_chisel.hh_core(114, 64, 64, 2, 256, 3)))
    sw2.close()
  }
}

object hh_core_chisel{
    class hh_core(name:Int,bw:Int, streaming_width:Int, ddot_st:Int, total_st:Int, SRAM_COUNT:Float)extends Module{
        val io = IO {
            new Bundle() {
                val clk = Input(Clock())
                val rst = Input(Bool())
                val hh_cnt = Input(UInt((16).W))
                val d1_rdy = Input(Bool())
                val d1_vld = Input(Bool())
               
                val vk1_vld = Input(Bool())
            
                val tk_vld = Input(Bool())

                val yjp_vld = Input(Bool())
                val yjp_total_vld = Input(Bool())
                val yj_sft = Input(Bool())

                val hh_st = Input(Bool())

                val x1_old_vld = Input(Bool())
                val xk_axpy_rotate = Input(Bool())
                val xk_d4_rotate = Input(Bool())


                val ddot2_in_sft = Input(Bool())
                val ddot1_in_sft = Input(Bool())



                val cnt_en_1 = Input(Vec(log2Ceil(total_st/ddot_st), Bool()))
                val reg_en_1 = Input(Vec(log2Ceil(total_st/ddot_st), Bool()))
                val cnt_en_2 = Input(Vec(log2Ceil(total_st/streaming_width), Bool()))
                val reg_en_2 = Input(Vec(log2Ceil(total_st/streaming_width), Bool()))

                val mem0_fi = Input(Bool())
                val mem1_fi = Input(Bool())
                val dmx0_mem_ena = Input(Bool())
                val dmx0_mem_wea = Input(UInt((total_st*4).W))
                val dmx0_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx0_mem_dina = Input(UInt((total_st*(bw/2)).W))
                val dmx0_mem_enb = Input(Bool())
                val dmx0_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx0_mem_doutb = Output(UInt((total_st*(bw/2)).W))
                val dmx1_mem_ena = Input(Bool())
                val dmx1_mem_wea = Input(UInt((total_st*4).W))
                val dmx1_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx1_mem_dina = Input(UInt((total_st*(bw/2)).W))
                val dmx1_mem_enb = Input(Bool())
                val dmx1_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx1_mem_doutb = Output(UInt((total_st*(bw/2)).W))
                val rtri_mem_ena = Input(Bool())
                val rtri_mem_wea = Input(UInt((total_st*4).W))
                val rtri_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val rtri_mem_dina = Input(UInt((total_st*(bw/2)).W))
                val rtri_mem_enb = Input(Bool())
                val rtri_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val rtri_mem_doutb = Output(UInt((total_st*(bw/2)).W))
                val hh_dout = Output(UInt((total_st*bw).W))
            }
        }

    


        withClockAndReset (io.clk, io.rst){

            val hh_dout = Wire(UInt((total_st*bw).W))

            val hh0_din_rdy = RegInit(0.B)
            val hh1_din_rdy = RegInit(0.B)
            val hh_din_update = RegInit(VecInit.fill(total_st)(0.U((bw).W)))
            val hh_dout_update = RegInit(VecInit.fill(total_st)(0.U((bw).W)))
            val hh_din = RegInit(0.U((streaming_width*bw).W))
            val mask = Wire(UInt((streaming_width*bw).W))
            
            hh0_din_rdy := (io.dmx0_mem_enb & io.rtri_mem_enb)
            hh1_din_rdy := (io.dmx1_mem_enb & io.rtri_mem_enb)
//////////////////////////////////////////////////////////////////////////////////////////

def counter2(max: UInt, enable: Bool) = {
            val x = RegInit(0.asUInt(max.getWidth.W))
            x := Mux(~enable,0.U,Mux(x === max, 0.U, x + 1.U))
            x
        }

        when(io.rst){
            mask :=  ~(0.U((total_st*bw).W))
        }.elsewhen(io.ddot1_in_sft){
            mask := (~(0.U((total_st*bw).W)))>>(io.hh_cnt*64.U)
        }.otherwise{
            mask := mask
        }

    when(hh0_din_rdy | hh1_din_rdy){
        for( i<- 0 until total_st){
            when(i.U > io.hh_cnt){
                hh_din_update((total_st-i-1).U) := 0.U 
            }.otherwise{
                hh_din_update((total_st-i-1).U) := (Cat(tri_mem_doutb,dmx_mem_doutb))(total_st*bw-1-(bw*i),total_st*bw-1-(bw*(i+1)))
            }}
    }.otherwise{
            hh_din_update := hh_din_update
    }


            val inputcounter = counter2((total_st/streaming_width).U, io.hh_st)
            val d1counter = counter2((total_st/ddot_st).U, io.ddot1_in_sft)

            when(io.hh_st){
                for(i <- 0 until streaming_width/ddot_st){
                hh_dout_update(total_st.U-1.U-(i.U+inputcounter*(streaming_width/ddot_st).U)) := u_hh_datapath.hh_dout((streaming_width*bw-(i*bw)-1),(streaming_width*bw-((i+1)*bw)))
                }
            }.otherwise{
                hh_dout_update := hh_dout_update
            }





            when(io.hh_cnt === 0.U & io.ddot1_in_sft){
            for( i<- 0 until total_st){
                when(i.U > io.hh_cnt){
                    hh_din_update((total_st-i-1).U) := 0.U 
                }.otherwise{
                    hh_din_update((total_st-i-1).U) := (Cat(tri_mem_doutb,dmx_mem_doutb))(total_st*bw-1-(bw*i),total_st*bw-1-(bw*(i+1)))
                }}

                hh_din <= hh_dout_update()
            }.elsewhen(io.ddot1_in_sft){
                hh_din <= hh_din
            }.otherwise{
                hh_din := 0.U
            }



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            val dmx_mem_doutb = Wire(UInt((total_st*bw/2).W))

            when(hh0_din_rdy){
                dmx_mem_doutb := io.dmx0_mem_doutb
            }.elsewhen(hh1_din_rdy){
                dmx_mem_doutb := io.dmx1_mem_doutb
            }.otherwise{
                dmx_mem_doutb := 0.U
            }

            val tri_mem_doutb = Wire(UInt((total_st*bw/2).W))

            tri_mem_doutb :=  io.rtri_mem_doutb

           
            


            //////////////////////////////////////////////////////////////////////////////////////////////////////////////

            val rtri_out_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val rtri_in_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val rtri_wea_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*(4/SRAM_COUNT)).toInt).W)))
            val rtri_layer = for(i <- 0 until SRAM_COUNT.toInt)yield{
            val rtri = Module(new simple_dual(name, bw,total_st,SRAM_COUNT)).io
            rtri
            }

            for(i <- 0 until SRAM_COUNT.toInt){
                rtri_in_vec(SRAM_COUNT.toInt - 1 -i) := io.rtri_mem_dina(((total_st*bw/2)-(i*(total_st*bw/(2*SRAM_COUNT)))-1).toInt, ((total_st*bw/2)-((i+1)*(total_st*bw/(2*SRAM_COUNT)))).toInt)
                rtri_wea_vec(SRAM_COUNT.toInt - 1 -i) :=io.rtri_mem_wea((total_st*(4-(i*4/SRAM_COUNT))-1).toInt,(total_st*(4-((i+1)*4/SRAM_COUNT))).toInt)
            }

            for(i <- 0 until SRAM_COUNT.toInt){
                rtri_layer(i).clka := io.clk
                rtri_layer(i).ena := io.rtri_mem_ena
                rtri_layer(i).wea := rtri_wea_vec(i)
                rtri_layer(i).addra := io.rtri_mem_addra
                rtri_layer(i).dina := rtri_in_vec(i)
                rtri_layer(i).clkb := io.clk
                rtri_layer(i).enb := io.rtri_mem_enb
                rtri_layer(i).addrb := io.rtri_mem_addrb
                rtri_out_vec(i) := rtri_layer(i).doutb
            }

            io.rtri_mem_doutb := rtri_out_vec.asUInt

            val dmx0_out_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val dmx0_in_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val dmx0_wea_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*(4/SRAM_COUNT)).toInt).W)))
            val dmx0_layer = for(i <- 0 until SRAM_COUNT.toInt)yield{
            val dmx0 = Module(new simple_dual(name, bw,total_st,SRAM_COUNT)).io
            dmx0
            }

            for(i <- 0 until SRAM_COUNT.toInt){
                dmx0_in_vec(SRAM_COUNT.toInt - 1 -i) := io.dmx0_mem_dina(((total_st*bw/2)-(i*(total_st*bw/(2*SRAM_COUNT)))-1).toInt, ((total_st*bw/2)-((i+1)*(total_st*bw/(2*SRAM_COUNT)))).toInt)
                dmx0_wea_vec(SRAM_COUNT.toInt - 1 -i) :=io.dmx0_mem_wea((total_st*(4-(i*4/SRAM_COUNT))-1).toInt,(total_st*(4-((i+1)*4/SRAM_COUNT))).toInt)
            }
            
            for(i <- 0 until SRAM_COUNT.toInt){
                dmx0_layer(i).clka := io.clk
                dmx0_layer(i).ena := io.dmx0_mem_ena
                dmx0_layer(i).wea := dmx0_wea_vec(i)
                dmx0_layer(i).addra := io.dmx0_mem_addra
                dmx0_layer(i).dina := dmx0_in_vec(i)
                dmx0_layer(i).clkb := io.clk
                dmx0_layer(i).enb := io.dmx0_mem_enb
                dmx0_layer(i).addrb := io.dmx0_mem_addrb
                dmx0_out_vec(i) := dmx0_layer(i).doutb
            }

            io.dmx0_mem_doutb := dmx0_out_vec.asUInt


            val dmx1_out_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val dmx1_in_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*bw/(2*SRAM_COUNT)).toInt).W)))
            val dmx1_wea_vec = Wire(Vec(SRAM_COUNT.toInt, UInt(width = ((total_st*(4/SRAM_COUNT)).toInt).W)))
            val dmx1_layer = for(i <- 0 until SRAM_COUNT.toInt)yield{
            val dmx1 = Module(new simple_dual(name, bw,total_st,SRAM_COUNT)).io
            dmx1
            }

            for(i <- 0 until SRAM_COUNT.toInt){
                dmx1_in_vec(SRAM_COUNT.toInt - 1 -i) := io.dmx1_mem_dina(((total_st*bw/2)-(i*(total_st*bw/(2*SRAM_COUNT)))-1).toInt, ((total_st*bw/2)-((i+1)*(total_st*bw/(2*SRAM_COUNT)))).toInt)
                dmx1_wea_vec(SRAM_COUNT.toInt - 1 -i) :=io.dmx1_mem_wea((total_st*(4-(i*4/SRAM_COUNT))-1).toInt,(total_st*(4-((i+1)*4/SRAM_COUNT))).toInt)
            }
            
            for(i <- 0 until SRAM_COUNT.toInt){
                dmx1_layer(i).clka := io.clk
                dmx1_layer(i).ena := io.dmx1_mem_ena
                dmx1_layer(i).wea := dmx1_wea_vec(i)
                dmx1_layer(i).addra := io.dmx1_mem_addra
                dmx1_layer(i).dina := dmx1_in_vec(i)
                dmx1_layer(i).clkb := io.clk
                dmx1_layer(i).enb := io.dmx1_mem_enb
                dmx1_layer(i).addrb := io.dmx1_mem_addrb
                dmx1_out_vec(i) := dmx1_layer(i).doutb
            }

            io.dmx1_mem_doutb := dmx1_out_vec.asUInt









/*


            val u_dmx0= Module(new simple_dual(name, bw,streaming_width)).io
            
            u_dmx0.clka := io.clk
            u_dmx0.ena := io.dmx0_mem_ena
            u_dmx0.wea := io.dmx0_mem_wea
            u_dmx0.addra := io.dmx0_mem_addra
            u_dmx0.dina := io.dmx0_mem_dina
            u_dmx0.clkb := io.clk
            u_dmx0.enb := io.dmx0_mem_enb
            u_dmx0.addrb := io.dmx0_mem_addrb
            io.dmx0_mem_doutb := u_dmx0.doutb 

            val u_dmx1= Module(new simple_dual(name, bw,streaming_width)).io
            u_dmx1.clka := io.clk
            u_dmx1.ena := io.dmx1_mem_ena
            u_dmx1.wea := io.dmx1_mem_wea
            u_dmx1.addra := io.dmx1_mem_addra
            u_dmx1.dina := io.dmx1_mem_dina
            u_dmx1.clkb := io.clk
            u_dmx1.enb := io.dmx1_mem_enb
            u_dmx1.addrb := io.dmx1_mem_addrb
            io.dmx1_mem_doutb := u_dmx1.doutb

            val u_rtri= Module(new simple_dual(name, bw,streaming_width)).io
            u_rtri.clka := io.clk
            u_rtri.ena := io.rtri_mem_ena
            u_rtri.wea := io.rtri_mem_wea
            u_rtri.addra := io.rtri_mem_addra
            u_rtri.dina := io.rtri_mem_dina
            u_rtri.clkb := io.clk
            u_rtri.enb := io.rtri_mem_enb
            u_rtri.addrb := io.rtri_mem_addrb
            io.rtri_mem_doutb := u_rtri.doutb
*/
            val u_hh_datapath= Module(new hh_datapath(name,bw,streaming_width,ddot_st, total_st))

            u_hh_datapath.clk := io.clk
            u_hh_datapath.rst := io.rst
            //u_hh_datapath.hh_cnt := io.hh_cnt
            u_hh_datapath.x1_old_vld := io.x1_old_vld

            u_hh_datapath.xk_axpy_rotate := io.xk_axpy_rotate
            u_hh_datapath.xk_d4_rotate := io.xk_d4_rotate

            u_hh_datapath.cnt_en_1 := io.cnt_en_1
            u_hh_datapath.reg_en_1 := io.reg_en_1
            u_hh_datapath.cnt_en_2 := io.cnt_en_2
            u_hh_datapath.reg_en_2 := io.reg_en_2

            u_hh_datapath.d1_rdy := io.d1_rdy
            u_hh_datapath.d1_vld := io.d1_vld
            
            u_hh_datapath.vk1_vld := io.vk1_vld
            
            u_hh_datapath.yjp_vld := io.yjp_vld
            u_hh_datapath.yj_sft := io.yj_sft

            u_hh_datapath.hh_din := hh_din

            
            when(io.yjp_vld){
                hh_dout:= Cat(hh_dout((total_st-streaming_width)*bw-1,0),u_hh_datapath.hh_dout)
            }.otherwise{
                hh_dout:= 0.U
            }
                
            
            
            when(io.yjp_total_vld){
                io.hh_dout:= hh_dout//>> (io.hh_cnt*bw.U)
            }.otherwise{
                io.hh_dout:= 0.U
            }
        }
    }
 
 /*
  class simple_dual(bw:Int, streaming_width:Int)extends BlackBox{
        val io = IO {
            new Bundle() {
                val clka = Input(Clock())
                val clkb = Input(Clock())
                val ena = Input(Bool())
                val enb = Input(Bool())
                val wea = Input(UInt((streaming_width*4).W))
                val addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dina = Input(UInt((streaming_width*bw/2).W))
                val doutb = Output(UInt((streaming_width*bw/2).W))
            }
        }
    }
        */

    class simple_dual(name:Int,bw:Int, streaming_width:Int, SRAM_COUNT:Float)extends Module{
        override def desiredName = s"simple_dual_${name}"
        val io = IO {
            new Bundle() {
                val clka = Input(Clock())
                val clkb = Input(Clock())
                val ena = Input(Bool())
                val enb = Input(Bool())
                val wea = Input(UInt((streaming_width*(4/SRAM_COUNT)).toInt.W))
                val addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dina = Input(UInt((streaming_width*bw/(2*SRAM_COUNT)).toInt.W))
                val doutb = Output(UInt((streaming_width*bw/(2*SRAM_COUNT)).toInt.W))
            }
        }
        withClock (io.clka){
            val doutb = Reg(UInt((streaming_width*bw/2).W))
            io.doutb := doutb
            val ram = Mem(streaming_width/2, UInt((streaming_width*bw/(2*SRAM_COUNT)).toInt.W)) 
            val ramtemp = Wire(Vec((streaming_width/(2*SRAM_COUNT)).toInt, UInt(width = (bw).W)))
            val dintemp = Wire(Vec((streaming_width/(2*SRAM_COUNT)).toInt, UInt(width = (bw).W)))
            when(io.ena){
                for(i <- 0 until (streaming_width/(2*SRAM_COUNT)).toInt){
                //dintemp(streaming_width/2-1-i) := (io.dina(streaming_width*bw/2-1-(i*bw),streaming_width*bw/2-((i+1)*bw)))&(Cat((io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4)))))
                //ramtemp(streaming_width/2-1-i) := ((ram(io.addra))(streaming_width*bw/2-1-(i*bw),streaming_width*bw/2-((i+1)*bw)))& ~(Cat((io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4)))))
                dintemp((streaming_width/(2*SRAM_COUNT)-1-i).toInt) := (io.dina((streaming_width*bw/(2*SRAM_COUNT)-1-(i*bw)).toInt,(streaming_width*bw/(2*SRAM_COUNT)-((i+1)*bw)).toInt))&(Cat((io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt))))
                ramtemp((streaming_width/(2*SRAM_COUNT)-1-i).toInt) := ((ram(io.addra))((streaming_width*bw/(2*SRAM_COUNT)-1-(i*bw)).toInt,(streaming_width*bw/(2*SRAM_COUNT)-((i+1)*bw)).toInt))& ~(Cat((io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt)),(io.wea((streaming_width*(4/SRAM_COUNT)-1-(i*8)).toInt,(streaming_width*(4/SRAM_COUNT)-((i+1)*8)).toInt))))  
                }
                ram.write(io.addra, ramtemp.asUInt + dintemp.asUInt)
            }.otherwise{
                for(i <- 0 until (streaming_width/(2*SRAM_COUNT)).toInt){
                dintemp(i) := 0.U
                ramtemp(i) := 0.U
                }
            }
            withClock (io.clkb){
                when(io.enb){
                    doutb := RegNext(ram.read(io.addrb))
                }
            }
        }
    }

 
}

