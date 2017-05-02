      IURUN=1                 ! run_data
      IUGRD=3                 ! model_grid (opened & closed in setdom.f)
      IUTAS=5                 ! init_tands (opened & closed in tands.f)
      IUUAV=7                 ! synop_wind (opened & closed in bcdata.f)
      IURRS=9                 ! restart (opened & closed in ecom3d.f)
	  ICONT=29
C
      IUPRT=10                ! gcmprt
      IUPLT=12                ! gcmplt
      IUTSR=14                ! gcmtsr
      IUWRS=16                ! startup (opened & closed in ecom3d.f)
      IUTRN=18                ! gcm_geom (opened & closed in transport.f)
C                             ! gcm_tran (opened & closed in transport.f)
C
      IUT90=90                ! elevation boundary conditions
      IUT91=91                ! river discharges
      IUT92=92                ! diffuser intake/outfall
      IUT93=93                ! meteorological data
      IUT94=94                ! temperature & salinity boundary conditions
C     ----------上述四个文件用于存储边界的临时变量值-----------


      IUT95=95                ! synoptic wind stress comp (opened in bcdata.f)
      IUT96=96                ! diffuser intake/outfall in loop
      IUT191=191              ! synoptic heat flux input file
      IUT192=192              ! synoptic heat flux (temp file)
      IUT193=193              ! synoptic wind velocity components 
c                               (opened in bcdata.f) used by wave model
C
C********************************************************************
C
C  THESE FILES ARE OPENED IN bcdata.f
C
C  DISSOLVED TRACER TRANSPORT
C
      IUT501=501                ! tracer conc. at open b.c.
      IUT601=601                ! tracer conc. at river discharges
      IUT98=98                  ! tracer conc. at diffuser intake/outfall
      IUT99=99                  ! tracer conc. at diffuser intake/outfall loop
      IUT701=701                ! tracer load at point source
C
C  SEDIMENT TRANSPORT
C
      IUT502=502                !  coh sed conc. at open b.c.
      IUT503=503                !  non-coh sed conc. at open b.c.
      IUT602=602                !  coh sed conc. at river discharges
      IUT603=603                !  non-coh sed conc.river discharges
      IUT702=702                !  coh sed conc. at Diffuser discharges
      IUT703=703                !  non-coh sed conc Diffuser discharges
C
C  PARTICLE-BOUND TRACER TRANSPORT
C
      IUT504=504                !  coh sed conc. at open b.c.
      IUT505=505                !  non-coh sed conc. at open b.c.
      IUT604=604                !  coh sed conc. at river discharges
      IUT605=605                !  non-coh sed conc.river discharges
      IUT704=704                !  coh sed conc. at Diffuser discharges
      IUT705=705                !  non-coh sed conc Diffuser discharges