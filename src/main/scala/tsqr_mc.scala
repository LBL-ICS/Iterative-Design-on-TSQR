/*************************************
 * 10-19-2023
 * Author: Blair Reasoner
 * Release version: 1
 * **********************************/

package tsqr_mc_package
import tsqr_fsm_package.chisel_fsm._
import tsqr_hh_core.hh_core_chisel._
import Binary_Modules.BinaryDesigns._
import FP_Modules.FloatingPointDesigns._
import chisel3._
import chisel3.util._
import Chisel.{log2Ceil, log2Floor}
import chiseltest.RawTester.test
import chisel3.tester._
import chisel3.{RawModule, withClockAndReset}
import java.io.PrintWriter
import scala.collection.mutable

object Main{

  def main(args: Array[String]) : Unit = {
    
    val startTimeMillis = System.currentTimeMillis()

    val sw2 = new PrintWriter("tile4_iterative.v")
    //tsqr_mc(name: Int, bw:Int, total_st:Int, CNT_WIDTH: Int, core_count: Int)
    sw2.println(getVerilogString(new tile4(19,64,16, 4, 2, 16, 1, 2)))
    //sw2.println(getVerilogString(new hh_core(64, 16, 16)))
    sw2.close()
    val endTimeMillis = System.currentTimeMillis()
    val durationSeconds = (endTimeMillis - startTimeMillis) / 1000

    print(durationSeconds, "\n");
  }
}

class tile4(name:Int,bw:Int, total_st:Int, streaming_width:Int,ddot_st:Int,CNT_WIDTH: Int, core_count: Int,SRAM_COUNT:Float)extends RawModule{
    val clk = IO(Input(Clock()))
    val rst = IO(Input(Bool()))
    val tsqr_en = IO(Input(Bool()))
    val tile_no = IO(Input(UInt((CNT_WIDTH).W)))
    val dma_mem_ena = IO(Input(UInt((core_count*3).W)))
    val dma_mem_wea = IO(Input(UInt((total_st*4).W)))
    val dma_mem_addra = IO(Input(UInt(((log2Ceil(total_st)-1)).W)))
    val dma_mem_dina = IO(Input(UInt((total_st*bw/2).W)))
    val dma_mem_enb = IO(Input(UInt((core_count*3).W)))
    val dma_mem_addrb = IO(Input(UInt(((log2Ceil(total_st)-1)).W)))
    val dma_mem_doutb = IO(Output(UInt((total_st*bw/2).W)))
    val mem0_fi_c = IO(Output((Vec(core_count, Bool()))))
    val mem1_fi_c = IO(Output((Vec(core_count, Bool()))))
    val tsqr_fi = IO(Output(Bool()))

    withClockAndReset(clk,rst){
   
    val  mx_cnt_c = Wire(Vec(core_count, UInt((CNT_WIDTH).W)))
    val  hh_cnt_c = Wire(Vec(core_count, UInt((CNT_WIDTH).W)))
    val  d1_rdy_c = Wire(Vec(core_count, Bool()))
    val  d1_vld_c = Wire(Vec(core_count, Bool()))
    
   
    val  vk1_vld_c = Wire(Vec(core_count, Bool()))
   
   
    val  tk_vld_c = Wire(Vec(core_count, Bool()))
    val  yjp_vld_c = Wire(Vec(core_count, Bool()))
    val  yj_sft_c = Wire(Vec(core_count, Bool()))



    val yjp_total_vld_c = Wire(Vec(core_count, Bool()))
    val x1_old_vld_c = Wire(Vec(core_count, Bool()))
    val xk_axpy_rotate_c = Wire(Vec(core_count, Bool()))
    val xk_d4_rotate_c = Wire(Vec(core_count, Bool()))
    val ddot1_in_sft_c = Wire(Vec(core_count, Bool()))
    val ddot2_in_sft_c = Wire(Vec(core_count, Bool()))
    val cnt_en_1_c = Wire(Vec(log2Ceil(total_st/ddot_st)*core_count, Bool()))
    val reg_en_1_c = Wire(Vec(log2Ceil(total_st/ddot_st)*core_count, Bool()))
    val cnt_en_2_c = Wire(Vec(log2Ceil(total_st/streaming_width)*core_count, Bool()))
    val reg_en_2_c = Wire(Vec(log2Ceil(total_st/streaming_width)*core_count, Bool()))
    
    val  hh_st_c = Wire(Vec(core_count, Bool()))
    val  tsqr_fi_c = Wire(Vec(core_count, Bool()))
    val  fsm_dmx0_mem_ena_c = Wire(Vec(core_count, Bool()))
    val  fsm_dmx0_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))
    val  fsm_dmx0_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  fsm_dmx0_mem_enb_c = Wire(Vec(core_count, Bool()))
    val  fsm_dmx0_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  fsm_dmx1_mem_ena_c = Wire(Vec(core_count, Bool()))
    val  fsm_dmx1_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))
    val  fsm_dmx1_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  fsm_dmx1_mem_enb_c = Wire(Vec(core_count, Bool()))
    val  fsm_dmx1_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  fsm_rtri_mem_ena_c = Wire(Vec(core_count, Bool()))
    val  fsm_rtri_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))
    val  fsm_rtri_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  fsm_rtri_mem_enb_c = Wire(Vec(core_count, Bool()))
    val  fsm_rtri_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val  hh_dout_c = Wire(Vec(core_count, UInt((total_st*bw).W)))
    val tsqr_en_c = Wire(Vec(core_count, Bool()))
    val tile_no_c = Wire(Vec(core_count, UInt(CNT_WIDTH.W)))
  
    val tsqr_fi_level_c = Reg(Vec(core_count, Bool()))

    for(i <- 1 until core_count){
        when(rst){
            tsqr_fi_level_c(i) := 0.B
        }.elsewhen(tsqr_fi_c(i)){
            tsqr_fi_level_c(i) := 1.B
        }.elsewhen(tsqr_fi){
            tsqr_fi_level_c(i) := 0.B
        }
    }

    tsqr_fi_level_c(0) := 0.B

    for(i <- 0 until core_count){
        tsqr_en_c(i) := (tsqr_en &(~(tsqr_fi_level_c(i))))
    }

    for(i <- 1 until core_count+1){
        when(tsqr_en_c(i-1)){
            if((i % 2) == 0){
                tile_no_c(i-1) := ((tile_no/core_count.U)-1.U)
            }else if((i % 4) == 1){
                if(i == 1){
                    tile_no_c(i-1) := ((tile_no/core_count.U)-1.U + log2Ceil(core_count).U)
                }else if(i%8 == 5){
                    tile_no_c(i-1) := ((tile_no/core_count.U)+1.U)
                }else if(i%16 == 9){
                    tile_no_c(i-1) := ((tile_no/core_count.U)+2.U)
                }else if(i%32 == 17){
                    tile_no_c(i-1) := ((tile_no/core_count.U)+3.U)
                }else if(i%64 == 33){
                    tile_no_c(i-1) := ((tile_no/core_count.U)+4.U)
                }else if(i%128 == 65){
                    tile_no_c(i-1) := ((tile_no/core_count.U)+5.U)
                }
            }else{
                tile_no_c(i-1) := (tile_no/core_count.U)
            }
        }.otherwise{
            tile_no_c(i-1) := 0.B
        }
    }

    when(tsqr_en_c(0)){
        tsqr_fi := RegNext(tsqr_fi_c(0))
    }.otherwise{
        tsqr_fi := 0.U
    }

    val rtri_mem_ena_c = Wire(Vec(core_count, Bool()))
    for(i <- 0 until core_count){
        rtri_mem_ena_c(i) := ((dma_mem_ena((core_count*3)-(i*3+1)))|(fsm_rtri_mem_ena_c(i)))
    }

    val dmx0_mem_ena_c = Wire(Vec(core_count, Bool()))
    val wr_dmx0_mem_ena = Wire(Vec(core_count-1, Bool()))
    for(i <- 1 until core_count+1){
        if((i % 2) == 0){
            dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1)))
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1)))
                }else if(core_count == 2){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1)))
                }else if(core_count == 4){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i)))
                }else if(core_count == 8){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2)))
                }else if(core_count == 16){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6)))
                }else if(core_count == 32){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6))|(wr_dmx0_mem_ena(i+14)))
                }else if(core_count == 64){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6))|(wr_dmx0_mem_ena(i+14))|(wr_dmx0_mem_ena(i+30)))
                }else if(core_count == 128){
                    dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6))|(wr_dmx0_mem_ena(i+14))|(wr_dmx0_mem_ena(i+30))|(wr_dmx0_mem_ena(i+62)))
                }
            }else if(i%8 == 5){
                dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i)))
            }else if(i%16 == 9){
                dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2)))
            }else if(i%32 == 17){
                dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6)))
            }else if(i%64 == 33){
                dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6))|(wr_dmx0_mem_ena(i+14)))
            }else if(i%128 == 65){
                dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1))|(wr_dmx0_mem_ena(i))|(wr_dmx0_mem_ena(i+2))|(wr_dmx0_mem_ena(i+6))|(wr_dmx0_mem_ena(i+14))|(wr_dmx0_mem_ena(i+30)))
            }
            
        }else{
            dmx0_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(2+((i-1)*3))))|(fsm_dmx0_mem_ena_c(i-1))|(wr_dmx0_mem_ena(i-1)))
        }
    }

    val dmx1_mem_ena_c = Wire(Vec(core_count, Bool()))
    val wr_dmx1_mem_ena = Wire(Vec(core_count-1, Bool()))
    for(i <- 1 until core_count+1){
        if((i % 2) == 0){
            dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1)))
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1)))
                }else if(core_count == 2){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1)))
                }else if(core_count == 4){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i)))
                }else if(core_count == 8){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2)))
                }else if(core_count == 16){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6)))
                }else if(core_count == 32){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6))|(wr_dmx1_mem_ena(i+14)))
                }else if(core_count == 64){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6))|(wr_dmx1_mem_ena(i+14))|(wr_dmx1_mem_ena(i+30)))
                }else if(core_count == 128){
                    dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6))|(wr_dmx1_mem_ena(i+14))|(wr_dmx1_mem_ena(i+30))|(wr_dmx1_mem_ena(i+62)))
                }
            }else if(i%8 == 5){
                dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i)))
            }else if(i%16 == 9){
                dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2)))
            }else if(i%32 == 17){
                dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6)))
            }else if(i%64 == 33){
                dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6))|(wr_dmx1_mem_ena(i+14)))
            }else if(i%128 == 65){
                dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1))|(wr_dmx1_mem_ena(i))|(wr_dmx1_mem_ena(i+2))|(wr_dmx1_mem_ena(i+6))|(wr_dmx1_mem_ena(i+14))|(wr_dmx1_mem_ena(i+30)))
            }
            
        }else{
            dmx1_mem_ena_c(i-1) := ((dma_mem_ena((core_count*3)-(i*3)))|(fsm_dmx1_mem_ena_c(i-1))|(wr_dmx1_mem_ena(i-1)))
        }
    }

    val rtri_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))
    val dmx0_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))
    val dmx1_mem_wea_c = Wire(Vec(core_count, UInt((total_st*4).W)))

    for(i <- 0 until core_count){
        when(dma_mem_ena((core_count*3)-(i*3+1))){
            rtri_mem_wea_c(i) := dma_mem_wea
        }.otherwise{
            rtri_mem_wea_c(i) := fsm_rtri_mem_wea_c(i)
        }
    }

    for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-(i*3-1))){
                dmx0_mem_wea_c(i-1) := dma_mem_wea
            }.otherwise{
                dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
            }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                    }.elsewhen(wr_dmx0_mem_ena(i+62)){
                        dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+63)
                    }.otherwise{
                        dmx0_mem_wea_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.otherwise{
                    dmx0_mem_wea_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.otherwise{
                    dmx0_mem_wea_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.otherwise{
                    dmx0_mem_wea_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                }.otherwise{
                    dmx0_mem_wea_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                }.elsewhen(wr_dmx0_mem_ena(i+30)){
                    dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                }.otherwise{
                    dmx0_mem_wea_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3-1))){
                dmx0_mem_wea_c(i-1) := dma_mem_wea
            }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                dmx0_mem_wea_c(i-1) := fsm_dmx0_mem_wea_c(i-1)
            }.elsewhen(wr_dmx0_mem_ena(i-1)){
                dmx0_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
            }.otherwise{
                dmx0_mem_wea_c(i-1) := 0.U
            }
        }
    }

 for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_wea_c(i-1) := dma_mem_wea
            }.otherwise{
                dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
            }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_wea_c(i-1) := dma_mem_wea
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                    }.elsewhen(wr_dmx1_mem_ena(i+62)){
                        dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+63)
                    }.otherwise{
                        dmx1_mem_wea_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.otherwise{
                    dmx1_mem_wea_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.otherwise{
                    dmx1_mem_wea_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.otherwise{
                    dmx1_mem_wea_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                }.otherwise{
                    dmx1_mem_wea_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_wea_c(i-1) := dma_mem_wea
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+7)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+15)
                }.elsewhen(wr_dmx1_mem_ena(i+30)){
                    dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i+31)
                }.otherwise{
                    dmx1_mem_wea_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_wea_c(i-1) := dma_mem_wea
            }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                dmx1_mem_wea_c(i-1) := fsm_dmx1_mem_wea_c(i-1)
            }.elsewhen(wr_dmx1_mem_ena(i-1)){
                dmx1_mem_wea_c(i-1) := fsm_rtri_mem_wea_c(i)
            }.otherwise{
                dmx1_mem_wea_c(i-1) := 0.U
            }
        }
    }

    val rtri_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val dmx0_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))    
    val dmx1_mem_addra_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))

    for(i <- 0 until core_count){
        when(dma_mem_ena((core_count*3)-(i*3+1))){
            rtri_mem_addra_c(i) := dma_mem_addra
        }.otherwise{
            rtri_mem_addra_c(i) := fsm_rtri_mem_addra_c(i)
        }
    }



    for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-(i*3-1))){
                dmx0_mem_addra_c(i-1) := dma_mem_addra
            }.otherwise{
                dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
            }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                    }.elsewhen(wr_dmx0_mem_ena(i+62)){
                        dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+63)
                    }.otherwise{
                        dmx0_mem_addra_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.otherwise{
                    dmx0_mem_addra_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.otherwise{
                    dmx0_mem_addra_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.otherwise{
                    dmx0_mem_addra_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                }.otherwise{
                    dmx0_mem_addra_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                }.elsewhen(wr_dmx0_mem_ena(i+30)){
                    dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                }.otherwise{
                    dmx0_mem_addra_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3-1))){
                dmx0_mem_addra_c(i-1) := dma_mem_addra
            }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                dmx0_mem_addra_c(i-1) := fsm_dmx0_mem_addra_c(i-1)
            }.elsewhen(wr_dmx0_mem_ena(i-1)){
                dmx0_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
            }.otherwise{
                dmx0_mem_addra_c(i-1) := 0.U
            }
        }
    }

 for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_addra_c(i-1) := dma_mem_addra
            }.otherwise{
                dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
            }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_addra_c(i-1) := dma_mem_addra
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                    }.elsewhen(wr_dmx1_mem_ena(i+62)){
                        dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+63)
                    }.otherwise{
                        dmx1_mem_addra_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.otherwise{
                    dmx1_mem_addra_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.otherwise{
                    dmx1_mem_addra_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.otherwise{
                    dmx1_mem_addra_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                }.otherwise{
                    dmx1_mem_addra_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_addra_c(i-1) := dma_mem_addra
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+1)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+3)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+7)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+15)
                }.elsewhen(wr_dmx1_mem_ena(i+30)){
                    dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i+31)
                }.otherwise{
                    dmx1_mem_addra_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_addra_c(i-1) := dma_mem_addra
            }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                dmx1_mem_addra_c(i-1) := fsm_dmx1_mem_addra_c(i-1)
            }.elsewhen(wr_dmx1_mem_ena(i-1)){
                dmx1_mem_addra_c(i-1) := fsm_rtri_mem_addra_c(i)
            }.otherwise{
                dmx1_mem_addra_c(i-1) := 0.U
            }
        }
    }

    val rtri_mem_dina_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))
    val dmx0_mem_dina_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))
    val dmx1_mem_dina_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))

    for(i <- 0 until core_count){
        when(dma_mem_ena((core_count*3)-(i*3+1))){
            rtri_mem_dina_c(i) := dma_mem_dina
        }.elsewhen(fsm_rtri_mem_ena_c(i)){
            rtri_mem_dina_c(i) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
        }.otherwise{
            rtri_mem_dina_c(i) := 0.U
        }
    }


        
    for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-((i*3)-1))){
                dmx0_mem_dina_c(i-1) := dma_mem_dina
            }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
     		dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
    	    }.otherwise{
                dmx0_mem_dina_c(i-1) :=0.U 
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
			            dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
		            }.otherwise{
                        dmx0_mem_dina_c(i-1) :=0.U 
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3-1))){
                        dmx0_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx0_mem_ena(i-1)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+2)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+6)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+14)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+30)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx0_mem_ena(i+62)){
                        dmx0_mem_dina_c(i-1) := hh_dout_c(i+63)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx0_mem_dina_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx0_mem_dina_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx0_mem_dina_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx0_mem_dina_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx0_mem_dina_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3-1))){
                    dmx0_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx0_mem_ena(i-1)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+2)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+6)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+14)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx0_mem_ena(i+30)){
                    dmx0_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx0_mem_dina_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3-1))){
                dmx0_mem_dina_c(i-1) := dma_mem_dina
            }.elsewhen(fsm_dmx0_mem_ena_c(i-1)){
                dmx0_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
            }.elsewhen(wr_dmx0_mem_ena(i-1)){
                dmx0_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
            }.otherwise{
                dmx0_mem_dina_c(i-1) := 0.U
            }
        }
    }

    for(i <- 1 until core_count+1){
         if((i % 2) == 0){
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_dina_c(i-1) := dma_mem_dina
            }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
     		dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
    	    }.otherwise{
                dmx1_mem_dina_c(i-1) :=0.U 
            }
        }else if((i % 4) == 1){
            if(i == 1){
                if(core_count == 1){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
			            dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
		            }.otherwise{
                        dmx1_mem_dina_c(i-1) :=0.U 
                    }
                }else if(core_count == 2){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }
                }else if(core_count == 4){
                   when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }
                }else if(core_count == 8){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }                   
                }else if(core_count == 16){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }                   
                }else if(core_count == 32){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }   
                }else if(core_count == 64){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    } 
                }else if(core_count == 128){
                    when(dma_mem_ena((core_count*3)-(i*3))){
                        dmx1_mem_dina_c(i-1) := dma_mem_dina
                    }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                    }.elsewhen(wr_dmx1_mem_ena(i-1)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+2)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+6)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+14)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+30)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                    }.elsewhen(wr_dmx1_mem_ena(i+62)){
                        dmx1_mem_dina_c(i-1) := hh_dout_c(i+63)(total_st*bw-1,total_st*bw/2)
                    }.otherwise{
                        dmx1_mem_dina_c(i-1) := 0.U
                    }
                }
            }else if(i%8 == 5){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx1_mem_dina_c(i-1) := 0.U
                }
            }else if(i%16 == 9){
                when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx1_mem_dina_c(i-1) := 0.U
                }
            }else if(i%32 == 17){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx1_mem_dina_c(i-1) := 0.U
                }  
            }else if(i%64 == 33){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx1_mem_dina_c(i-1) := 0.U
                } 
            }else if(i%128 == 65){
               when(dma_mem_ena((core_count*3)-(i*3))){
                    dmx1_mem_dina_c(i-1) := dma_mem_dina
                }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
                }.elsewhen(wr_dmx1_mem_ena(i-1)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+1)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+2)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+3)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+6)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+7)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+14)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+15)(total_st*bw-1,total_st*bw/2)
                }.elsewhen(wr_dmx1_mem_ena(i+30)){
                    dmx1_mem_dina_c(i-1) := hh_dout_c(i+31)(total_st*bw-1,total_st*bw/2)
                }.otherwise{
                    dmx1_mem_dina_c(i-1) := 0.U
                }
            }
        }else{
            when(dma_mem_ena((core_count*3)-(i*3))){
                dmx1_mem_dina_c(i-1) := dma_mem_dina
            }.elsewhen(fsm_dmx1_mem_ena_c(i-1)){
                dmx1_mem_dina_c(i-1) := hh_dout_c(i-1)(total_st*bw/2-1,0)
            }.elsewhen(wr_dmx1_mem_ena(i-1)){
                dmx1_mem_dina_c(i-1) := hh_dout_c(i)(total_st*bw-1,total_st*bw/2)
            }.otherwise{
                dmx1_mem_dina_c(i-1) := 0.U
            }
        }
    }

    val rtri_mem_enb_c = Wire(Vec(core_count, Bool()))
    val dmx0_mem_enb_c = Wire(Vec(core_count, Bool()))
    val dmx1_mem_enb_c = Wire(Vec(core_count, Bool()))

    for(i <- 0 until core_count){
        rtri_mem_enb_c(i) := ((dma_mem_enb((core_count*3)-(i*3+1)))|(fsm_rtri_mem_enb_c(i)))
        dmx0_mem_enb_c(i) := ((dma_mem_enb((core_count*3)-(i*3+2)))|(fsm_dmx0_mem_enb_c(i)))
        dmx1_mem_enb_c(i) := ((dma_mem_enb((core_count*3)-((i+1)*3)))|(fsm_dmx1_mem_enb_c(i)))
    }



    val rtri_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val dmx0_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))
    val dmx1_mem_addrb_c = Wire(Vec(core_count, UInt((log2Ceil(total_st)-1).W)))

    for(i <- 0 until core_count){
        when(dma_mem_enb((core_count*3)-(i*3+1))){
            rtri_mem_addrb_c(i) := dma_mem_addrb
        }.otherwise{
            rtri_mem_addrb_c(i) := fsm_rtri_mem_addrb_c(i)
        }
    }

    for(i <- 0 until core_count){
        when(dma_mem_enb((core_count*3)-(i*3+2))){
            dmx0_mem_addrb_c(i) := dma_mem_addrb
        }.otherwise{
            dmx0_mem_addrb_c(i) := fsm_dmx0_mem_addrb_c(i)
        }
    }

    for(i <- 0 until core_count){
        when(dma_mem_enb((core_count*3)-((i+1)*3))){
            dmx1_mem_addrb_c(i) := dma_mem_addrb
        }.otherwise{
            dmx1_mem_addrb_c(i) := fsm_dmx1_mem_addrb_c(i)
        }
    }

    val rtri_mem_doutb_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))
    val dmx0_mem_doutb_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))
    val dmx1_mem_doutb_c = Wire(Vec(core_count, UInt((total_st*bw/2).W)))

   /* when(rst){
        dma_mem_doutb := 0.U
    }.otherwise{
        if((PriorityEncoderOH(dma_mem_enb)+1.U)%3.U == 0){
            dma_mem_doutb := rtri_mem_doutb_c(core_count.U-(((PriorityEncoderOH(dma_mem_enb)+1.U))/3.U))
        }else if((PriorityEncoder(dma_mem_enb)+1.U)%3.U == 2){
            dma_mem_doutb := dmx0_mem_doutb_c(core_count.U-(((PriorityEncoderOH(dma_mem_enb)+2.U))/3.U))
        }else{
            dma_mem_doutb := dmx1_mem_doutb_c(core_count.U-(((PriorityEncoderOH(dma_mem_enb)+3.U))/3.U))
        }
    }
*/
    when(rst){
        dma_mem_doutb := 0.U
        }.elsewhen(dma_mem_enb(2) === 1.U){
            dma_mem_doutb := rtri_mem_doutb_c(0)}
      .elsewhen(dma_mem_enb(1) === 1.U){
            dma_mem_doutb := dmx0_mem_doutb_c(0)
        }.elsewhen(dma_mem_enb(0) === 1.U){
            dma_mem_doutb := dmx1_mem_doutb_c(0)
        }.otherwise{
            dma_mem_doutb := 0.U
        }

       /* for(i <- 0 until core_count){
        when(rst){
            dma_mem_doutb := 0.U
            
    }.elsewhen( dma_mem_enb((core_count*3)-(i*3+1)) ){
            dma_mem_doutb := rtri_mem_doutb_c(i)}
      .elsewhen(dma_mem_enb((core_count*3)-(i*3+2))){
            dma_mem_doutb := dmx0_mem_doutb_c(i)
        }.elsewhen(dma_mem_enb((core_count*3)-(i*3+3))){
            dma_mem_doutb := dmx1_mem_doutb_c(i)
        }.otherwise{
            dma_mem_doutb := 1.U
        }}
*/
        
    /* when(rst){
        dma_mem_doutb := 0.U
    }.elsewhen(dma_mem_enb(5) === 1.U){
            dma_mem_doutb := rtri_mem_doutb_c(0)}
      .elsewhen(dma_mem_enb(4) === 1.U){
            dma_mem_doutb := dmx0_mem_doutb_c(0)
        }.elsewhen(dma_mem_enb(3) === 1.U){
            dma_mem_doutb := dmx1_mem_doutb_c(0)
        }.elsewhen(dma_mem_enb(2) === 1.U){
            dma_mem_doutb := rtri_mem_doutb_c(1)}
      .elsewhen(dma_mem_enb(1) === 1.U){
            dma_mem_doutb := dmx0_mem_doutb_c(1)
        }.elsewhen(dma_mem_enb(0) === 1.U){
            dma_mem_doutb := dmx1_mem_doutb_c(1)
        }.otherwise{
            dma_mem_doutb := 0.U
        }*/
    
    
    for(i <- 0 until core_count-1){
        if(((i+2) % 2) == 0){
            wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext((mx_cnt_c(i+1)))===(tile_no/core_count.U-2.U))&(~(tile_no(log2Ceil(core_count))))
            wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext((mx_cnt_c(i+1)))===(tile_no/core_count.U-2.U))&((tile_no(log2Ceil(core_count))))
        }else if((i+2) % 4 == 1){
            if((i+2)%8 == 5){
                wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U))&(~(tile_no(log2Ceil(core_count))))
                wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U))&((tile_no(log2Ceil(core_count))))
            }else if((i+2)%16 == 9){
                wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+1.U)&((tile_no(log2Ceil(core_count))))
                wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+1.U)&(~(tile_no(log2Ceil(core_count))))
            }else if((i+2)%32 == 17){
                wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+2.U)&(~(tile_no(log2Ceil(core_count))))
                wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+2.U)&((tile_no(log2Ceil(core_count))))
            }else if((i+2)%64 == 33){
                wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+3.U)&((tile_no(log2Ceil(core_count))))
                wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+3.U)&(~(tile_no(log2Ceil(core_count))))
            }else if((i+2)%128 == 65){
                wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+4.U)&(~(tile_no(log2Ceil(core_count))))
                wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U)+4.U)&((tile_no(log2Ceil(core_count))))
            }
        }else{
            wr_dmx1_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U-1.U))&((tile_no(log2Ceil(core_count))))
            wr_dmx0_mem_ena(i) := (rtri_mem_ena_c(i+1))&(RegNext(mx_cnt_c(i+1))===(tile_no/core_count.U-1.U))&(~(tile_no(log2Ceil(core_count))))
        }
    }
    //val fsms = Vector.fill(core_count)(Module(new fsm(bw, total_st, CNT_WIDTH)).io)
    val fsms = Vector.fill(core_count)(Module(new fsm(bw, total_st, streaming_width, ddot_st, CNT_WIDTH)))
    val cores = Vector.fill(core_count)(Module(new hh_core( name,bw,streaming_width,ddot_st, total_st, SRAM_COUNT)).io)
    
    for(i <- 0 until core_count){
        fsms(i).clk := clk
        fsms(i).rst := rst
        fsms(i).tsqr_en := tsqr_en_c(i)
        fsms(i).tile_no := tile_no_c(i)
        hh_cnt_c(i) := fsms(i).hh_cnt
        mx_cnt_c(i) := fsms(i).mx_cnt
        d1_rdy_c(i) := fsms(i).d1_rdy
        d1_vld_c(i) := fsms(i).d1_vld
       
        
        vk1_vld_c(i) := fsms(i).vk1_vld
      
     
        tk_vld_c(i) := fsms(i).tk_vld
       
      
        yjp_vld_c(i) := fsms(i).yjp_vld
        yj_sft_c(i) := fsms(i).yj_sft

        yjp_total_vld_c(i) := fsms(i).yjp_total_vld
        x1_old_vld_c(i)    := fsms(i).x1_old_vld
        xk_axpy_rotate_c(i):=fsms(i).xk_axpy_rotate
        xk_d4_rotate_c(i) := fsms(i).xk_d4_rotate
        ddot1_in_sft_c(i) := fsms(i).ddot1_in_sft
        ddot2_in_sft_c(i) := fsms(i).ddot2_in_sft

        for(k <- 0 until log2Ceil(total_st/ddot_st)){
            cnt_en_1_c(i*log2Ceil(total_st/ddot_st)+k)    := fsms(i).cnt_en_1(k)
            reg_en_1_c(i*log2Ceil(total_st/ddot_st)+k)    := fsms(i).reg_en_1(k)
        }
        for(l<- 0 until log2Ceil(total_st/streaming_width)){
            cnt_en_2_c(i*log2Ceil(total_st/streaming_width)+l)    := fsms(i).cnt_en_2(l)
            reg_en_2_c(i*log2Ceil(total_st/streaming_width)+l)    := fsms(i).reg_en_2(l)
        }
        
        hh_st_c(i) := fsms(i).hh_st
        mem0_fi_c(i) := fsms(i).mem0_fi
        mem1_fi_c(i) := fsms(i).mem1_fi
        tsqr_fi_c(i) := fsms(i).tsqr_fi
        fsm_dmx0_mem_ena_c(i) := fsms(i).dmx0_mem_ena
        fsm_dmx0_mem_wea_c(i) := fsms(i).dmx0_mem_wea
        fsm_dmx0_mem_addra_c(i) := fsms(i).dmx0_mem_addra
        fsm_dmx0_mem_enb_c(i) := fsms(i).dmx0_mem_enb
        fsm_dmx0_mem_addrb_c(i) := fsms(i).dmx0_mem_addrb
        fsm_dmx1_mem_ena_c(i) := fsms(i).dmx1_mem_ena
        fsm_dmx1_mem_wea_c(i) := fsms(i).dmx1_mem_wea
        fsm_dmx1_mem_addra_c(i) := fsms(i).dmx1_mem_addra
        fsm_dmx1_mem_enb_c(i) := fsms(i).dmx1_mem_enb
        fsm_dmx1_mem_addrb_c(i) := fsms(i).dmx1_mem_addrb
        fsm_rtri_mem_ena_c(i) := fsms(i).rtri_mem_ena
        fsm_rtri_mem_wea_c(i) := fsms(i).rtri_mem_wea
        fsm_rtri_mem_addra_c(i) := fsms(i).rtri_mem_addra
        fsm_rtri_mem_enb_c(i) := fsms(i).rtri_mem_enb
        fsm_rtri_mem_addrb_c(i) := fsms(i).rtri_mem_addrb

        cores(i).clk := clk
        cores(i).rst := rst
        cores(i).hh_cnt := hh_cnt_c(i)
        cores(i).d1_rdy := d1_rdy_c(i)
        cores(i).d1_vld := d1_vld_c(i)
       
        
        cores(i).vk1_vld := vk1_vld_c(i)
       
   
        cores(i).tk_vld := tk_vld_c(i)
       
        
        cores(i).yjp_vld := yjp_vld_c(i)
        cores(i).yj_sft := yj_sft_c(i)

        cores(i).yjp_total_vld := yjp_total_vld_c(i)
        cores(i).x1_old_vld    := x1_old_vld_c(i)
        cores(i).xk_axpy_rotate:= xk_axpy_rotate_c(i)
        cores(i).xk_d4_rotate  := xk_d4_rotate_c(i)
        cores(i).ddot1_in_sft  := ddot1_in_sft_c(i)
        cores(i).ddot2_in_sft  := ddot2_in_sft_c(i)


        for(k <- 0 until log2Ceil(total_st/ddot_st)){
            cores(i).cnt_en_1(k):=cnt_en_1_c(i*log2Ceil(total_st/ddot_st)+k) 
            cores(i).reg_en_1(k):=reg_en_1_c(i*log2Ceil(total_st/ddot_st)+k) 
        }
        for(l<- 0 until log2Ceil(total_st/streaming_width)){
            cores(i).cnt_en_2(l):=cnt_en_2_c(i*log2Ceil(total_st/streaming_width)+l)
            cores(i).cnt_en_2(l):=reg_en_2_c(i*log2Ceil(total_st/streaming_width)+l)
        }

  
        cores(i).hh_st := hh_st_c(i)
        cores(i).mem0_fi := mem0_fi_c(i)
        cores(i).mem1_fi := mem1_fi_c(i)
        cores(i).dmx0_mem_ena := dmx0_mem_ena_c(i)
        cores(i).dmx0_mem_wea := dmx0_mem_wea_c(i)
        cores(i).dmx0_mem_addra := dmx0_mem_addra_c(i)
        cores(i).dmx0_mem_dina := dmx0_mem_dina_c(i)
        cores(i).dmx0_mem_enb := dmx0_mem_enb_c(i)
        cores(i).dmx0_mem_addrb := dmx0_mem_addrb_c(i)
        cores(i).dmx1_mem_ena := dmx1_mem_ena_c(i)
        cores(i).dmx1_mem_wea := dmx1_mem_wea_c(i)
        cores(i).dmx1_mem_addra := dmx1_mem_addra_c(i)
        cores(i).dmx1_mem_dina := dmx1_mem_dina_c(i)
        cores(i).dmx1_mem_enb := dmx1_mem_enb_c(i)
        cores(i).dmx1_mem_addrb := dmx1_mem_addrb_c(i)
        cores(i).rtri_mem_ena := rtri_mem_ena_c(i)
        cores(i).rtri_mem_wea := rtri_mem_wea_c(i)
        cores(i).rtri_mem_addra := rtri_mem_addra_c(i)
        cores(i).rtri_mem_dina := rtri_mem_dina_c(i)
        cores(i).rtri_mem_enb := rtri_mem_enb_c(i)
        cores(i).rtri_mem_addrb := rtri_mem_addrb_c(i)
        hh_dout_c(i) := cores(i).hh_dout
        rtri_mem_doutb_c(i) := cores(i).rtri_mem_doutb
        dmx0_mem_doutb_c(i) := cores(i).dmx0_mem_doutb
        dmx1_mem_doutb_c(i) := cores(i).dmx1_mem_doutb
    }
 }
}
/****************************************************************************
 * Black Boxes for the hh_core and fsm module. 
 * **************************************************************************/

 
/*
class hh_core(bw:Int, total_st:Int, CNT_WIDTH: Int) extends BlackBox{
    val io = IO(new Bundle {
        val clk = Input(Clock())
                val rst = Input(Bool())
                val hh_cnt = Input(UInt((CNT_WIDTH).W))
                val d1_rdy = Input(Bool())
                val d1_vld = Input(Bool())
                val d2_rdy = Input(Bool())
                val d2_vld = Input(Bool())
                val vk1_rdy = Input(Bool())
                val vk1_vld = Input(Bool())
                val d3_rdy = Input(Bool())
                val d3_vld = Input(Bool())
                val tk_rdy = Input(Bool())
                val tk_vld = Input(Bool())
                val d4_rdy = Input(Bool())
                val d4_vld = Input(Bool())
                val d5_rdy = Input(Bool())
                val d5_vld = Input(Bool())
                val yjp_rdy = Input(Bool())
                val yjp_vld = Input(Bool())
                val yj_sft = Input(Bool())
                val d4_sft = Input(Bool())
                val hh_st = Input(Bool())
                val mem0_fi = Input(Bool())
                val mem1_fi = Input(Bool())
                val dmx0_mem_ena = Input(Bool())
                val dmx0_mem_wea = Input(UInt((total_st*2).W))
                val dmx0_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx0_mem_dina = Input(UInt((total_st*16).W))
                val dmx0_mem_enb = Input(Bool())
                val dmx0_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx0_mem_doutb = Output(UInt((total_st*16).W))
                val dmx1_mem_ena = Input(Bool())
                val dmx1_mem_wea = Input(UInt((total_st*2).W))
                val dmx1_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx1_mem_dina = Input(UInt((total_st*16).W))
                val dmx1_mem_enb = Input(Bool())
                val dmx1_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val dmx1_mem_doutb = Output(UInt((total_st*16).W))
                val rtri_mem_ena = Input(Bool())
                val rtri_mem_wea = Input(UInt((total_st*2).W))
                val rtri_mem_addra = Input(UInt((log2Ceil(total_st)-1).W))
                val rtri_mem_dina = Input(UInt((total_st*16).W))
                val rtri_mem_enb = Input(Bool())
                val rtri_mem_addrb = Input(UInt((log2Ceil(total_st)-1).W))
                val rtri_mem_doutb = Output(UInt((total_st*16).W))
                val hh_dout = Output(UInt((total_st*bw).W))
    })
}*/
/*
class fsm(bw:Int, total_st:Int, CNT_WIDTH: Int) extends BlackBox{
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val rst = Input(Bool())
        val tsqr_en = Input(Bool())
        val tile_no = Input(UInt((CNT_WIDTH).W))
        val hh_cnt = Output(UInt((CNT_WIDTH).W))
        val mx_cnt = Output(UInt((CNT_WIDTH).W))
        val d1_rdy = Output(Bool())
        val d1_vld = Output(Bool())
        val d2_rdy = Output(Bool())
        val d2_vld = Output(Bool())
        val vk1_rdy = Output(Bool())
        val vk1_vld = Output(Bool())
        val d3_rdy = Output(Bool())
        val d3_vld = Output(Bool())
        val tk_rdy = Output(Bool())
        val tk_vld = Output(Bool())
        val d4_rdy = Output(Bool())
        val d4_vld = Output(Bool())
        val d5_rdy = Output(Bool())
        val d5_vld = Output(Bool())
        val yjp_rdy = Output(Bool())
        val yjp_vld = Output(Bool())
        val yj_sft = Output(Bool())
        val d4_sft = Output(Bool())
        val hh_st = Output(Bool())
        val mem0_fi = Output(Bool())
        val mem1_fi = Output(Bool())
        val tsqr_fi = Output(Bool())

        val dmx0_mem_ena = Output(Bool())
        val dmx0_mem_wea = Output(UInt((total_st*2).W))
        val dmx0_mem_addra = Output(UInt((log2Ceil(total_st)-1).W))
        val dmx0_mem_enb = Output(Bool())
        val dmx0_mem_addrb = Output(UInt((log2Ceil(total_st)-1).W))
        val dmx1_mem_ena = Output(Bool())
        val dmx1_mem_wea = Output(UInt((total_st*2).W))
        val dmx1_mem_addra = Output(UInt((log2Ceil(total_st)-1).W))
        val dmx1_mem_enb = Output(Bool())
        val dmx1_mem_addrb = Output(UInt((log2Ceil(total_st)-1).W))
        val rtri_mem_ena = Output(Bool())
        val rtri_mem_wea = Output(UInt((total_st*2).W))
        val rtri_mem_addra = Output(UInt((log2Ceil(total_st)-1).W))
        val rtri_mem_enb = Output(Bool())
        val rtri_mem_addrb = Output(UInt((log2Ceil(total_st)-1).W))  
    })
}
*/