      SUBROUTINE INIFNAME
      USE DFLIB   !�������		  
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

      !     ---------�˴�fname,PATHID���и�ֵ���㣬��˱���������������ĺ���---------

      INCLUDE 'INC\PATHID.INC'  


  

      
      
      FNAME(40)='\ECOM_P\geo_org\ZONECHANGE.dat'      !ecom�߽���Ϣ

      FNAME(3)='\ECOM_P\geo_org\dam_info.dat'       !���й����Ϣ
      

C         ==================================================
      FNAME(24)='\ECOM_P\dat_org\control.dat'



      FNAME(4)='\ECOM_P\dat_org\monitor_res.dat'
      FNAME(19)='\ECOM_P\dat_org\qcheck_moni.dat'
      FNAME(25)='\ECOM_P\dat_in\'
      
      
      
      

      FNAME(1)='\RCA_P\EUTRO\dataorg\monitor_res.dat'
      
      FNAME(2)='\SWAT\dat_org\monitor_res.DAT'   !�ֿ���Ϣ�ļ�
      
      FNAME(6)='\RCA_P\EUTRO\dataorg\view_wq_3d.DAT'
      
      FNAME(5)='\SWAT\MONI_RES\MONI'
      
 
     
      FNAME(7)='\RCA_P\EUTRO\TEC_3D\TEC3D'
      
      
      
      FNAME(13)='\ECOM_P\dat_org\view_wq_3d.DAT'   !RCA      
      FNAME(14)='\ECOM_P\tec_3d\TEC3D'      !ecom�߽���Ϣ    
     
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
     



C         ===========��ԭʼRUN_DATA��Ϊ�����ļ�����=========


      
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


      FNAMEIN(37)='\time_metini.DAT'   !�����ļ�
      
      FNAMEIN(6)='\accident_in.dat'   
      
      
      FNAMEIN(38)='\index_float.DAT'   !
      FNAMEIN(39)='\time_float.DAT'   !�����ļ�
      
      
      FNAMEIN(40)='\dam_cont.dat'   !���δ�ӿ���
      FNAMEIN(41)='\time_dam_info.dat'   !���ڴ���¶��ζ�






      
      
CW--------ƽ̨��ʾ-----------      
      NAME_RES(1)="EUVZ"   !����U,V,Z
      NAME_RES(2)="ETEM"   !�¶�
      NAME_RES(3)="ELBT"   !ˮλ
      
      
CW--------RCAʹ��-----------      
      NAME_RES(4)="RCAQ"   !����QX,QY,QZ
      NAME_RES(5)="RCAE"   !ͨ��EX,EY,EZ
      NAME_RES(6)="RCAM"   !����DUMMY
      NAME_RES(7)="RCAD"   !ˮλES,ED
      NAME_RES(8)="RCAT"   !�¶�
      
 
      
C-----------------------------------------------------------------------            




      FNAME(18)='\ECOM_P\TEMP\tectmp'


      FNAME(39)='\ECOM_P\tec_res\tec'      !ecom���
      
      
      FNAME(48)='\ECOM_P\tec_all\tec'      !ecom���
      
      
      FNAME(47)='\RCA_P\EUTRO\tec_all\tec'      !rcaȫ���
      
      
      
      FNAME(48)='\SWAT\hru_city.dat'      !rcaȫ���
      
 
   
CW    =============���ӽ����ȡ��·�����е���===============
CW    ======================================================

      
      DO I = 1,48
          FNAME(I)=trim(USERPATH)//FNAME(I)    !�ַ����ӣ���//
      END DO

      END 
