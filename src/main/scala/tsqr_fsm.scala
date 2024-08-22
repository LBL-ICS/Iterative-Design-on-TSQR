/*************************************
 * 10-19-2023
 * Author: Blair Reasoner
 * Release version: 1
 * **********************************/

package tsqr_fsm_package
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
    val sw2 = new PrintWriter("fsm.v")

   // sw2.println(getVerilogString(new chisel_fsm.fsm(32, 16, 16)))
    sw2.close()
  }
}
object chisel_fsm{
class fsm(bw:Int, total_st:Int,streaming_width:Int,ddot_st:Int, CNT_WIDTH: Int) extends RawModule{

    val clk = IO(Input(Clock()))
    val rst = IO(Input(Bool()))
    val tsqr_en = IO(Input(Bool()))
    val tile_no = IO(Input(UInt((CNT_WIDTH).W)))
    val hh_cnt = IO(Output((UInt((CNT_WIDTH).W))))
    val mx_cnt = IO(Output((UInt((CNT_WIDTH).W))))
    val d1_rdy = IO(Output(Bool()))
    val d1_vld = IO(Output(Bool()))
    val vk1_vld = IO(Output(Bool()))
    val tk_vld = IO(Output(Bool()))
    val yjp_vld = IO(Output(Bool()))
    val yj_sft = IO(Output(Bool()))


    val yjp_total_vld = IO(Output(Bool()))
    val x1_old_vld = IO(Output(Bool()))
    val xk_axpy_rotate = IO(Output(Bool()))
    val xk_d4_rotate = IO(Output(Bool()))
    val ddot1_in_sft = IO(Output(Bool()))
    val ddot2_in_sft = IO(Output(Bool()))
    val cnt_en_1 = IO(Output(Vec(log2Ceil(total_st/ddot_st), Bool())))
    val reg_en_1 = IO(Output(Vec(log2Ceil(total_st/ddot_st), Bool())))
    val cnt_en_2 = IO(Output(Vec(log2Ceil(total_st/streaming_width), Bool())))
    val reg_en_2 = IO(Output(Vec(log2Ceil(total_st/streaming_width), Bool())))
   
    val hh_st = IO(Output(Bool()))
    val mem0_fi = IO(Output(Bool()))
    val mem1_fi = IO(Output(Bool()))
    val tsqr_fi = IO(Output(Bool()))
    val dmx0_mem_ena = IO(Output(Bool()))
    val dmx0_mem_wea = IO(Output(UInt((total_st*4).W)))
    val dmx0_mem_addra = IO(Output(UInt((log2Ceil(total_st)-1).W)))
    val dmx0_mem_enb = IO(Output(Bool()))
    val dmx0_mem_addrb = IO(Output(UInt((log2Ceil(total_st)-1).W)))
    val dmx1_mem_ena = IO(Output(Bool()))
    val dmx1_mem_wea = IO(Output(UInt((total_st*4).W)))
    val dmx1_mem_addra = IO(Output(UInt((log2Ceil(total_st)-1).W)))
    val dmx1_mem_enb = IO(Output(Bool()))
    val dmx1_mem_addrb = IO(Output(UInt((log2Ceil(total_st)-1).W)))
    val rtri_mem_ena = IO(Output(Bool()))
    val rtri_mem_wea = IO(Output(UInt((total_st*4).W)))
    val rtri_mem_addra = IO(Output(UInt((log2Ceil(total_st)-1).W)))
    val rtri_mem_enb = IO(Output(Bool()))
    val rtri_mem_addrb = IO(Output(UInt((log2Ceil(total_st)-1).W)))

    withClockAndReset(clk,rst){
        val DDOT1_CY = log2Ceil(ddot_st*2)*13+11 + 13*(((log2Ceil(total_st/ddot_st)-1))+1)
        val DDOT2_CY = log2Ceil(streaming_width)*13+24 + 13*(((log2Ceil(total_st/streaming_width)-1))+1)
        val HQR3_CY = 139 //sqrt 
        //val HQR3_CY = 29   
        val HQR5_CY = 83// 330 
        val HQR6_CY = 36
        //val HQR5_CY = 36
        val HQR7_CY = 24//129
        //val HQR7_CY = 29
        val HQR10_CY = 23
        val HQR11_CY = 36
        val YJ_SFT_NO = DDOT2_CY + HQR10_CY  //+ HQR7_CY+1
       
        val MEM_RD_CY = 2
        val VK_CY = DDOT1_CY + HQR3_CY + HQR5_CY
        val TK_CY = VK_CY + HQR6_CY + HQR7_CY
        val TR_CY_MACRO = DDOT2_CY + HQR10_CY + HQR11_CY //- MEM_RD_CY + HQR7_CY
        val HH_CY = MEM_RD_CY + VK_CY + TR_CY_MACRO + total_st/streaming_width //+ 1//edit 1-19-24
 
        val hh_en = RegInit(0.B)

        when(mem0_fi | mem1_fi){
            hh_en := 0.U
        }.elsewhen(tsqr_en){
            hh_en := 1.U
        }


        def counter2(max: UInt, enable: Bool) = {
            val x = RegInit(0.asUInt(max.getWidth.W))
            x := Mux(~enable,max,Mux(x === max, 0.U, x + 1.U))
            x
        }

        def pulse2(n: UInt, enable: Bool) = counter2((n),enable) === (0.U)












        val cnt = counter2((HH_CY.U),hh_en)
       
        val nxt_hh_cnt = Reg(UInt(CNT_WIDTH.W))
        val nxt_mx_cnt = Reg(UInt(CNT_WIDTH.W))


        when((cnt === (HH_CY).U)&(hh_cnt === (total_st.U/2.U-1.U))){
            nxt_hh_cnt := 0.U
        }.elsewhen(hh_en & (cnt === (HH_CY).U)){
            nxt_hh_cnt := hh_cnt + 1.U
        }.otherwise{
            nxt_hh_cnt := hh_cnt
        }

        when((hh_cnt === (total_st/2-1).U)&(mx_cnt === (tile_no ))&(cnt ===  (HH_CY).U)){//tile_no -1
            nxt_mx_cnt := 0.U
        }.elsewhen( (hh_cnt === (total_st/2-1).U)&(cnt === (HH_CY).U -1.U)){//hh_en &//change 3/20/24
            nxt_mx_cnt := mx_cnt + 1.U
        }.otherwise{
            nxt_mx_cnt := mx_cnt
        }

        when(rst){
            hh_cnt := 0.U
            mx_cnt := 0.U
        }.otherwise{
            hh_cnt := nxt_hh_cnt
            mx_cnt := nxt_mx_cnt
        }

      
        val tr_cnt_en = RegInit(0.B)
        val tr_cnt = RegInit(0.U(CNT_WIDTH.W))
        val rd_mem_fst = RegInit(0.B)
        val wr_mem_st = RegInit(0.B)
        val rd_mem_st = RegInit(0.B)


        rd_mem_fst := (tsqr_en & (~hh_en))
        wr_mem_st := hh_en & (tr_cnt === (TR_CY_MACRO-1).U)
        //hh_st := hh_en & (tr_cnt === (TR_CY_MACRO).U)
        rd_mem_st := hh_en & (cnt === (VK_CY-2).U)
        
        when(~(hh_cnt === 0.U) & (cnt <= (total_st/streaming_width).U)){
            hh_st := 1.B
        }.otherwise{
            hh_st := 0.B
        }


        val tr_cy = RegInit(0.U(CNT_WIDTH.W))

        when(rd_mem_st){
            tr_cy := ((total_st/2).U - hh_cnt)*(total_st/streaming_width).U//iterations*columns left
        }.otherwise{
            tr_cy :=  tr_cy
        }


    for(i <- 0 until (log2Ceil((total_st/ddot_st))-1)){
        when(rst){
        reg_en_1(i) := 0.B
    }.elsewhen(cnt === (log2Ceil(streaming_width+1)*13+11 + 13*(i+1)-(1<<(i+1))+1).U ){
        reg_en_1(i) := 1.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11 + 13*(i+1)-(1<<(i+1))+1).U+(total_st/ddot_st).U*(((total_st/2).U - hh_cnt)))){
        reg_en_1(i) := 0.B
    }.otherwise{
        reg_en_1(i) := RegNext(reg_en_1(i))
    }
    when(rst){
        cnt_en_1(i) := 0.B
    }.elsewhen(cnt === (log2Ceil(streaming_width+1)*13+11 + 13*(i)+1).U ){
        cnt_en_1(i) := 1.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11 + 13*(i)+1).U+(total_st/ddot_st).U*(((total_st/2).U - hh_cnt)))){
        cnt_en_1(i) := 0.B
    }.otherwise{
        cnt_en_1(i) := RegNext(cnt_en_1(i))
    }
}


when(rst){
        cnt_en_1((log2Ceil((total_st/ddot_st))-1)) := 0.B
    }.elsewhen(cnt === (log2Ceil(streaming_width+1)*13+11 + 13*((log2Ceil((total_st/ddot_st))-1))+1).U ){
        cnt_en_1((log2Ceil((total_st/ddot_st))-1)) := 1.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11 + 13*((log2Ceil((total_st/ddot_st))-1))+1+(total_st/ddot_st)).U+(total_st/ddot_st).U*(((total_st/2).U - hh_cnt)))){
        cnt_en_1((log2Ceil((total_st/ddot_st))-1)) := 0.B
    }.otherwise{
        cnt_en_1((log2Ceil((total_st/ddot_st))-1)) := RegNext(cnt_en_1((log2Ceil((total_st/ddot_st))-1)))
    }

when(rst){
        reg_en_1((log2Ceil((total_st/ddot_st))-1)) := 0.B
    }.elsewhen(cnt === (log2Ceil(streaming_width+1)*13+11 + 13*(((log2Ceil((total_st/ddot_st))-1))+1)-(1<<(((log2Ceil((total_st/ddot_st))-1))+1))+1).U ){
        reg_en_1(((log2Ceil((total_st/ddot_st))-1))) := 1.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11 + 13*(((log2Ceil((total_st/ddot_st))-1))+1)-(1<<(((log2Ceil((total_st/ddot_st))-1))+1))+1+(total_st/ddot_st)*2-1).U+(total_st/ddot_st).U*(((total_st/2).U - hh_cnt)))){
        reg_en_1(((log2Ceil((total_st/ddot_st))-1))) := 0.B
    }.otherwise{
        reg_en_1(((log2Ceil((total_st/ddot_st))-1))) := RegNext(reg_en_1(((log2Ceil((total_st/ddot_st))-1))))
    }

       


    for(i <- 0 until (log2Ceil((total_st/streaming_width))-1)){

        when(rst){
            cnt_en_2(i) := 0.B
        }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*(i)+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U ){
            cnt_en_2(i) := 1.B
        }.elsewhen(cnt === (((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*(i)+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U +tr_cy)){
            cnt_en_2(i) := 0.B
        }.otherwise{
            cnt_en_2(i) := RegNext(cnt_en_2(i))
        }

        when(rst){
            reg_en_2(i) := 0.B
        }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*(i+1)-(1<<(i+1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U ){
            reg_en_2(i) := 1.B
        }.elsewhen(cnt === (((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*(i+1)-(1<<(i+1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U+tr_cy)){
            reg_en_2(i) := 0.B
        }.otherwise{
            reg_en_2(i) := RegNext(reg_en_2(i))
        }


    }

    when(rst){
        cnt_en_2((log2Ceil((total_st/streaming_width))-1)) := 0.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*((log2Ceil((total_st/streaming_width))-1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U ){
        cnt_en_2((log2Ceil((total_st/streaming_width))-1)) := 1.B
    }.elsewhen((cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*((log2Ceil((total_st/streaming_width))-1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1+(total_st/streaming_width))+1).U+tr_cy)){
        cnt_en_2((log2Ceil((total_st/streaming_width))-1)) := 0.B
    }.otherwise{
        cnt_en_2((log2Ceil((total_st/streaming_width))-1)) := RegNext(cnt_en_2((log2Ceil((total_st/streaming_width))-1)))
    }

    when(rst){
        reg_en_2((log2Ceil((total_st/streaming_width))-1)) := 0.B
    }.elsewhen(cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*((log2Ceil((total_st/streaming_width))-1)+1)-(1<<((log2Ceil((total_st/streaming_width))-1)+1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1).U ){
        reg_en_2((log2Ceil((total_st/streaming_width))-1)) := 1.B
    }.elsewhen((cnt === ((log2Ceil(streaming_width+1)*13+11)*2+HQR5_CY+HQR3_CY + 13*((log2Ceil((total_st/streaming_width))-1)+1)-(1<<((log2Ceil((total_st/streaming_width))-1)+1))+(log2Ceil((total_st/streaming_width))*13  + (total_st/ddot_st)-1)+1+(total_st/streaming_width)*2-1).U+tr_cy)){
        reg_en_2((log2Ceil((total_st/streaming_width))-1)) := 0.B
    }.otherwise{
        reg_en_2((log2Ceil((total_st/streaming_width))-1)) := RegNext(reg_en_2((log2Ceil((total_st/streaming_width))-1)))
    }




        val hh_fi = Wire(Bool())
        hh_fi := (tr_cnt === (TR_CY_MACRO.U + tr_cy ))//+1

       
        when(hh_fi){
            tr_cnt := 0.U
        }.elsewhen(tr_cnt_en){
            tr_cnt := tr_cnt + 1.U
        }.otherwise{
            tr_cnt := tr_cnt
        }

    
        when(hh_fi){
            tr_cnt_en := 0.B
        }.elsewhen( hh_en & (cnt === (VK_CY).U)){
            tr_cnt_en := 1.B
        }

        
        //val tr_cnt_en_2 = Reg(Bool())


     /*  
        when(rst){
            tr_cnt_en_2 := 0.B
        }.elsewhen(hh_fi){
            tr_cnt_en_2 := 0.B
        }.elsewhen( hh_en & (cnt === (VK_CY+2).U)){
            tr_cnt_en_2 := 1.B
        }
    */

        d1_rdy := RegNext(hh_en & cnt > 1.U & cnt < (total_st/ddot_st+1).U)//+2
        d1_vld := RegNext(hh_en & (cnt === (DDOT1_CY).U))//+2
        
        vk1_vld := RegNext(hh_en & (cnt === (VK_CY).U))
        
        tk_vld := RegNext(hh_en & (cnt === (TK_CY).U))

        ddot1_in_sft := (hh_en & (cnt >= 0.U) & cnt <= (total_st/ddot_st).U)
        ddot2_in_sft := (tr_cnt_en & tr_cnt <= (tr_cy))

        xk_axpy_rotate := (tr_cnt_en &  tr_cnt < ( DDOT2_CY.U + HQR10_CY.U  + tr_cy) & (tr_cnt >= ( DDOT2_CY.U + HQR10_CY.U)))
        xk_d4_rotate := (tr_cnt_en &  tr_cnt < (tr_cy))

        


        
       
        yjp_vld := tr_cnt_en &  tr_cnt < ( DDOT2_CY.U + HQR10_CY.U  + HQR11_CY.U + tr_cy) & (tr_cnt >= ( DDOT2_CY.U + HQR10_CY.U + HQR11_CY.U))

        yjp_total_vld := pulse2((total_st/streaming_width).U, tr_cnt_en &  tr_cnt>=(total_st/streaming_width).U &  tr_cnt < ( DDOT2_CY.U + HQR10_CY.U  + HQR11_CY.U + tr_cy) & (tr_cnt >= ( DDOT2_CY.U + HQR10_CY.U + HQR11_CY.U)))

        x1_old_vld := hh_en & (cnt === (DDOT1_CY + HQR3_CY + 23).U)
        
        //yj_sft :=RegNext(tr_cnt_en & (tr_cnt < ( YJ_SFT_NO.U + tr_cy )))
        yj_sft := RegNext(tr_cnt_en) & (tr_cnt < ( YJ_SFT_NO.U + tr_cy ))
      
        
        val dmx0_mem_enb_reg = RegInit(0.B)
        val dmx1_mem_enb_reg = RegInit(0.B)
        val rtri_mem_enb_reg = RegInit(0.B)
        val dmx0_mem_ena_reg = RegInit(0.B)
        val dmx1_mem_ena_reg = RegInit(0.B)
        val rtri_mem_ena_reg = RegInit(0.B)
        val rd_dmx0_en = RegInit(0.B)
        val rd_dmx1_en = RegInit(0.B)
        val rd_rtri_en = RegInit(0.B)

  


        rd_dmx0_en := pulse2((total_st/streaming_width-1).U,~mx_cnt(0) & (cnt >= (VK_CY-1).U & cnt < (VK_CY).U + tr_cy + 1.U))
        rd_dmx1_en := pulse2((total_st/streaming_width-1).U,mx_cnt(0) & (cnt >= (VK_CY-1).U & cnt < (VK_CY).U + tr_cy + 1.U))
        rd_rtri_en := pulse2((total_st/streaming_width-1).U,(cnt >=(VK_CY-1).U  & cnt < (VK_CY).U + tr_cy + 1.U))



        dmx0_mem_enb_reg := (~mx_cnt(0) & (rd_mem_fst )) | rd_dmx0_en 
        dmx1_mem_enb_reg := (mx_cnt(0) & (rd_mem_fst)) | rd_dmx1_en 
        rtri_mem_enb_reg := rd_mem_fst | rd_rtri_en
    

        val dmx0_mem_addrb_reg = RegInit(0.U((log2Ceil(total_st)-1).W))
        val dmx1_mem_addrb_reg = RegInit(0.U((log2Ceil(total_st)-1).W))
        val rtri_mem_addrb_reg = RegInit(0.U((log2Ceil(total_st)-1).W))

        when(rd_mem_fst){
            dmx0_mem_addrb_reg := 0.U
        }.elsewhen(~mx_cnt(0) & (hh_en & (cnt === (VK_CY).U))){
            dmx0_mem_addrb_reg:= hh_cnt
        }.elsewhen(rd_dmx0_en){
            dmx0_mem_addrb_reg := dmx0_mem_addrb_reg + 1.U
        }.otherwise{
            dmx0_mem_addrb_reg := dmx0_mem_addrb_reg
        }

        when(rd_mem_fst){
            dmx1_mem_addrb_reg := 0.U
        }.elsewhen(mx_cnt(0) & (hh_en & (cnt === (VK_CY).U))){
            dmx1_mem_addrb_reg := hh_cnt
        }.elsewhen(rd_dmx1_en){
            dmx1_mem_addrb_reg := dmx1_mem_addrb_reg + 1.U
        }.otherwise{
            dmx1_mem_addrb_reg := dmx1_mem_addrb_reg
        }

        when(rd_mem_fst){
            rtri_mem_addrb_reg := 0.U
        }.elsewhen((hh_en & (cnt === (VK_CY).U))){
            rtri_mem_addrb_reg := hh_cnt
        }.elsewhen(rd_rtri_en){
            rtri_mem_addrb_reg := rtri_mem_addrb_reg + 1.U
        }.otherwise{
            rtri_mem_addrb_reg := rtri_mem_addrb_reg
        }

        val dmx0_mem_wea_reg = RegInit(~0.U((total_st*4).W))
        val dmx1_mem_wea_reg = RegInit(~0.U((total_st*4).W))
        val rtri_mem_wea_reg = RegInit(~0.U((total_st*4).W))




        dmx0_mem_ena_reg := pulse2((total_st/streaming_width-1).U,~mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U+ tr_cy)))//pulse2
        dmx1_mem_ena_reg := pulse2((total_st/streaming_width-1).U,mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U + tr_cy)))
        rtri_mem_ena_reg := pulse2((total_st/streaming_width-1).U,(tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ( (TR_CY_MACRO).U+ tr_cy)))
        

        dmx0_mem_wea := dmx0_mem_wea_reg
        dmx1_mem_wea := dmx1_mem_wea_reg
        rtri_mem_wea := rtri_mem_wea_reg
        
    

        when(~mx_cnt(0) & wr_mem_st & hh_cnt>=1.U){
            rtri_mem_wea_reg := rtri_mem_wea_reg >> (8.U)
        }.otherwise{
            dmx0_mem_wea_reg := dmx0_mem_wea_reg
            dmx1_mem_wea_reg := dmx1_mem_wea_reg
            rtri_mem_wea_reg := rtri_mem_wea_reg
        }

        val dmx0_mem_addra_reg = Reg(UInt((log2Ceil(total_st)-1).W))
        val dmx1_mem_addra_reg = Reg(UInt((log2Ceil(total_st)-1).W))
        val rtri_mem_addra_reg = Reg(UInt((log2Ceil(total_st)-1).W))

        when(~mx_cnt(0) & (hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            dmx0_mem_addra_reg := hh_cnt
        }.elsewhen(pulse2((total_st/streaming_width-1).U,~mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U+ tr_cy)))){//pulse2
            dmx0_mem_addra_reg := dmx0_mem_addra_reg + 1.U
        }.otherwise{
            dmx0_mem_addra_reg := dmx0_mem_addra_reg
        }

        when(mx_cnt(0) & (hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            dmx1_mem_addra_reg := hh_cnt
        }.elsewhen(pulse2((total_st/streaming_width-1).U,mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U+ tr_cy)))){//pulse2
            dmx1_mem_addra_reg := dmx1_mem_addra_reg + 1.U
        }.otherwise{
            dmx1_mem_addra_reg := dmx1_mem_addra_reg
        }

        when((hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            rtri_mem_addra_reg := hh_cnt
        }.elsewhen(pulse2((total_st/streaming_width-1).U,(tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ( (TR_CY_MACRO).U+ tr_cy)))){//pulse2
            rtri_mem_addra_reg := rtri_mem_addra_reg + 1.U
        }.otherwise{
            rtri_mem_addra_reg := rtri_mem_addra_reg
        }


    dmx0_mem_addrb := dmx0_mem_addrb_reg
    dmx1_mem_addrb := dmx1_mem_addrb_reg
    rtri_mem_addrb := rtri_mem_addrb_reg
    dmx0_mem_addra := dmx0_mem_addra_reg
    dmx1_mem_addra := dmx1_mem_addra_reg
    rtri_mem_addra := rtri_mem_addra_reg
//switched 3/20
   mem0_fi := ~mx_cnt(0) & (tr_cnt === (TR_CY_MACRO.U + tr_cy - 1.U)) & (tr_cy === 1.U)
   mem1_fi := mx_cnt(0) &(tr_cnt === (TR_CY_MACRO.U + tr_cy -1.U)) & (tr_cy === 1.U)

    tsqr_fi := (mem0_fi | mem1_fi) & (mx_cnt === (tile_no-1.U))//-1

   
    dmx0_mem_enb := dmx0_mem_enb_reg
    dmx1_mem_enb := dmx1_mem_enb_reg
    rtri_mem_enb := rtri_mem_enb_reg
    dmx0_mem_ena := dmx0_mem_ena_reg
    dmx1_mem_ena := dmx1_mem_ena_reg
    rtri_mem_ena := rtri_mem_ena_reg

    }
}
}