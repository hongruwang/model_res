      SUBROUTINE INIFNAME
      USE DFLIB   !传入参数		  
      INCLUDE 'INC\comdeck.INC'
      INCLUDE 'INC\ECOM3D.INC'
      INCLUDE 'INC\FHCALC.INC'
      INCLUDE 'INC\FLUX.INC'
      INCLUDE 'INC\FMIN.INC'
      INCLUDE 'INC\HITFLX.INC'
      INCLUDE 'INC\INDEX.INC'
      INCLUDE 'INC\PARA.INC'
      INCLUDE 'INC\PROFE.INC'
      INCLUDE 'INC\PROFQ.INC'
      INCLUDE 'INC\PROFSED.INC'
      INCLUDE 'INC\PROFXY.INC'
      INCLUDE 'INC\SEDIC.INC'
      INCLUDE 'INC\TANDS.INC'
      INCLUDE 'INC\TRANINP.INC'
      INCLUDE 'INC\TRANSPORT.INC'      
      INCLUDE 'INC\WREAL.INC' 
      INCLUDE 'INC\WVULP.INC'   
      INCLUDE 'INC\ZM.INC'         

      !     ---------此处fname,PATHID中有赋值计算，因此必须放在所有声明的后面---------

      INCLUDE 'INC\PATHID.INC'  


  

      
      
      FNAME(40)='\ECOM_P\geo_org\ZONECHANGE.dat'      !ecom边界信息

      FNAME(3)='\ECOM_P\geo_org\dam_info.dat'       !大坝泄流信息
      

C         ==================================================
      FNAME(24)='\ECOM_P\dat_org\control.dat'



      FNAME(4)='\ECOM_P\dat_org\monitor_res.dat'
      FNAME(19)='\ECOM_P\dat_org\qcheck_moni.dat'
      FNAME(25)='\ECOM_P\dat_in\'
      
      
      
      

      FNAME(1)='\RCA_P\EUTRO\dataorg\monitor_res.dat'
      
      FNAME(2)='\SWAT\dat_org\monitor_res.DAT'   !分块信息文件
      
      FNAME(6)='\RCA_P\EUTRO\dataorg\view_wq_3d.DAT'
      
      FNAME(5)='\SWAT\MONI_RES\MONI'
      
 
     
      FNAME(7)='\RCA_P\EUTRO\TEC_3D\TEC3D'
      
      
      
      FNAME(13)='\ECOM_P\dat_org\view_wq_3d.DAT'   !RCA      
      FNAME(14)='\ECOM_P\tec_3d\TEC3D'      !ecom边界信息    
     
      FNAME(9)='\ECOM_P\MONI_RES\MONI'
      FNAME(8)='\RCA_P\EUTRO\MONI_RES\MONI'
      

      
      


     
      FNAME(17)='\ECOM_P\RCA_RES\' 

      FNAME(15)='\RCA_P\EUTRO\TEC_RES\TEC'

C		  FNAME(14)='\ECOM_P\run\startup'

      FNAME(10)='\SWAT\OUTPUT\output.rch'
      FNAME(11)='\SWAT\OUTPUT\output.hru'
      FNAME(12)='\SWAT\OUTPUT\output.sub'

 

      
      FNAME(16)='\ECOM_P\RCA_RES\gcm_qdiff'  !RCA
      
       


      FNAME(20)='\ECOM_P\hotfiles\HOTECOM'
     
      
       FNAME(23)='\ECOM_P\geo_org\model_grid\model_grid' 
       FNAME(21)= '\ECOM_P\HOT_RES\HOTECOM'
       FNAME(22)= '\ECOM_P\HOT_RES\HOTINFO.DAT'
     



C         ===========将原始RUN_DATA简化为以下文件读入=========


      
      FNAME(26)='\ECOM_P\dat_in\'
      FNAME(35)='\ECOM_P\dat_in\'
      FNAME(36)='\ECOM_P\dat_in\'

      FNAME(27)='\ECOM_P\dat_in\'
      FNAME(28)='\ECOM_P\dat_in\'


      FNAME(29)='\ECOM_P\dat_in\'
      FNAME(31)='\ECOM_P\dat_in\'

      FNAME(30)='\ECOM_P\dat_in\'
      FNAME(32)='\ECOM_P\dat_in\'

      FNAME(33)='\ECOM_P\dat_in\'
      FNAME(34)='\ECOM_P\dat_in\'
      FNAME(37)='\ECOM_P\dat_in\'   
      FNAME(41)='\ECOM_P\dat_in\'   
      FNAME(42)='\ECOM_P\dat_in\' 
      FNAME(43)='\ECOM_P\dat_in\' 
      FNAME(44)='\ECOM_P\dat_in\' 
      FNAME(45)='\ECOM_P\dat_in\' 
      FNAME(46)='\ECOM_P\dat_in\' 


C-----------------------------------------------------------------------            
      FNAMEIN(25)='\bc_line_ps.dat'
      FNAMEIN(26)='\bc_line_nps.dat'



      FNAMEIN(35)='\index_down.DAT'
      FNAMEIN(36)='\time_elev_down.DAT'
      FNAMEIN(42)='\bc_line_down.DAT'
      
      FNAMEIN(43)='\bc_line_out.DAT'

      FNAMEIN(27)='\index_up.DAT'
      FNAMEIN(28)='\time_qc_up.DAT'
      FNAMEIN(19)='\bc_line_up.DAT'


      FNAMEIN(29)='\index_ps.DAT'
      FNAMEIN(31)='\time_ps_in.DAT'

      FNAMEIN(30)='\index_nps.DAT'
      FNAMEIN(32)='\time_nps_in.DAT'

      FNAMEIN(33)='\index_loop.DAT'
      FNAMEIN(34)='\time_loop_in.DAT'


      FNAMEIN(37)='\time_metini.DAT'   !气象文件
      
      FNAMEIN(6)='\accident_in.dat'   
      
      
      FNAMEIN(38)='\index_float.DAT'   !
      FNAMEIN(39)='\time_float.DAT'   !气象文件
      
      
      FNAMEIN(40)='\dam_cont.dat'   !下游大坝控制
      FNAMEIN(41)='\time_dam_info.dat'   !出口大坝温度盐度






      
      
CW--------平台演示-----------      
      NAME_RES(1)="EUVZ"   !流速U,V,Z
      NAME_RES(2)="ETEM"   !温度
      NAME_RES(3)="ELBT"   !水位
      
      
CW--------RCA使用-----------      
      NAME_RES(4)="RCAQ"   !流量QX,QY,QZ
      NAME_RES(5)="RCAE"   !通量EX,EY,EZ
      NAME_RES(6)="RCAM"   !流量DUMMY
      NAME_RES(7)="RCAD"   !水位ES,ED
      NAME_RES(8)="RCAT"   !温度
      
 
      
C-----------------------------------------------------------------------            




      FNAME(18)='\ECOM_P\TEMP\tectmp'


      FNAME(39)='\ECOM_P\tec_res\tec'      !ecom结果
      
      
      FNAME(48)='\ECOM_P\tec_all\tec'      !ecom结果
      
      
      FNAME(47)='\RCA_P\EUTRO\tec_all\tec'      !rca全结果
      
      
      
      FNAME(48)='\SWAT\hru_city.dat'      !rca全结果
      
 
   
CW    =============将从界面获取的路径进行叠加===============
CW    ======================================================

      
      DO I = 1,48
          FNAME(I)=trim(USERPATH)//FNAME(I)    !字符叠加，用//
      END DO

      END 
