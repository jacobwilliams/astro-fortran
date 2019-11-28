      PROGRAM T_SOFA_F
!+
!  - - - - - - - - -
!   t _ s o f a _ f
!  - - - - - - - - -
!
!  Validate the SOFA subprograms.
!
!  Each SOFA routine is at least called and a usually quite basic test
!  is performed.  Successful completion is signalled by a confirming
!  message.  Failure of a given routine or group of routines results
!  in error messages.
!
!  All messages go to standard output.
!
!  This revision:  2018 December 6
!
!  SOFA release 2019-07-22
!
!  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!
!----------------------------------------------------------------------

      use astro_module

      IMPLICIT NONE

      LOGICAL STATUS
      INTEGER JESTAT


!  Preset the status to success.
      STATUS = .TRUE.

!  Test all of the SOFA subroutines and functions.
      CALL T_A2AF ( STATUS )
      CALL T_A2TF ( STATUS )
      CALL T_AB ( STATUS )
      CALL T_AE2HD ( STATUS )
      CALL T_AF2A ( STATUS )
      CALL T_ANP ( STATUS )
      CALL T_ANPM ( STATUS )
      CALL T_APCG ( STATUS )
      CALL T_APCG13 ( STATUS )
      CALL T_APCI ( STATUS )
      CALL T_APCI13 ( STATUS )
      CALL T_APCO ( STATUS )
      CALL T_APCO13 ( STATUS )
      CALL T_APCS ( STATUS )
      CALL T_APCS13 ( STATUS )
      CALL T_APER ( STATUS )
      CALL T_APER13 ( STATUS )
      CALL T_APIO ( STATUS )
      CALL T_APIO13 ( STATUS )
      CALL T_ATCI13 ( STATUS )
      CALL T_ATCIQ ( STATUS )
      CALL T_ATCIQN ( STATUS )
      CALL T_ATCIQZ ( STATUS )
      CALL T_ATCO13 ( STATUS )
      CALL T_ATIC13 ( STATUS )
      CALL T_ATICQ ( STATUS )
      CALL T_ATICQN ( STATUS )
      CALL T_ATIO13 ( STATUS )
      CALL T_ATIOQ ( STATUS )
      CALL T_ATOC13 ( STATUS )
      CALL T_ATOI13 ( STATUS )
      CALL T_ATOIQ ( STATUS )
      CALL T_BI00 ( STATUS )
      CALL T_BP00 ( STATUS )
      CALL T_BP06 ( STATUS )
      CALL T_BPN2XY ( STATUS )
      CALL T_C2I00A ( STATUS )
      CALL T_C2I00B ( STATUS )
      CALL T_C2I06A ( STATUS )
      CALL T_C2IBPN ( STATUS )
      CALL T_C2IXY ( STATUS )
      CALL T_C2IXYS ( STATUS )
      CALL T_C2S ( STATUS )
      CALL T_C2T00A ( STATUS )
      CALL T_C2T00B ( STATUS )
      CALL T_C2T06A ( STATUS )
      CALL T_C2TCIO ( STATUS )
      CALL T_C2TEQX ( STATUS )
      CALL T_C2TPE ( STATUS )
      CALL T_C2TXY ( STATUS )
      CALL T_CAL2JD ( STATUS )
      CALL T_CP ( STATUS )
      CALL T_CPV ( STATUS )
      CALL T_CR ( STATUS )
      CALL T_D2DTF ( STATUS )
      CALL T_D2TF ( STATUS )
      CALL T_DAT ( STATUS )
      CALL T_DTDB ( STATUS )
      CALL T_DTF2D ( STATUS )
      CALL T_ECEQ06 ( STATUS )     !
      CALL T_ECM06 ( STATUS )
      CALL T_EE00 ( STATUS )
      CALL T_EE00A ( STATUS )
      CALL T_EE00B ( STATUS )
      CALL T_EE06A ( STATUS )
      CALL T_EECT00 ( STATUS )
      CALL T_EFORM ( STATUS )
      CALL T_EO06A ( STATUS )
      CALL T_EORS ( STATUS )
      CALL T_EPB ( STATUS )
      CALL T_EPB2JD ( STATUS )
      CALL T_EPJ ( STATUS )
      CALL T_EPJ2JD ( STATUS )
      CALL T_EPV00 ( STATUS )
      CALL T_EQEC06 ( STATUS )
      CALL T_EQEQ94 ( STATUS )
      CALL T_ERA00 ( STATUS )
      CALL T_FAD03 ( STATUS )
      CALL T_FAE03 ( STATUS )
      CALL T_FAF03 ( STATUS )
      CALL T_FAJU03 ( STATUS )
      CALL T_FAL03 ( STATUS )
      CALL T_FALP03 ( STATUS )
      CALL T_FAMA03 ( STATUS )
      CALL T_FAME03 ( STATUS )
      CALL T_FANE03 ( STATUS )
      CALL T_FAOM03 ( STATUS )
      CALL T_FAPA03 ( STATUS )
      CALL T_FASA03 ( STATUS )
      CALL T_FAUR03 ( STATUS )
      CALL T_FAVE03 ( STATUS )
      CALL T_FK425 ( STATUS )
      CALL T_FK45Z ( STATUS )
      CALL T_FK524 ( STATUS )
      CALL T_FK52H ( STATUS )
      CALL T_FK54Z ( STATUS )
      CALL T_FK5HIP ( STATUS )
      CALL T_FK5HZ ( STATUS )
      CALL T_FW2M ( STATUS )
      CALL T_FW2XY ( STATUS )
      CALL T_G2ICRS ( STATUS )
      CALL T_GC2GD ( STATUS )
      CALL T_GC2GDE ( STATUS )
      CALL T_GD2GC ( STATUS )
      CALL T_GD2GCE ( STATUS )
      CALL T_GMST00 ( STATUS )
      CALL T_GMST06 ( STATUS )
      CALL T_GMST82 ( STATUS )
      CALL T_GST00A ( STATUS )
      CALL T_GST00B ( STATUS )
      CALL T_GST06 ( STATUS )
      CALL T_GST06A ( STATUS )
      CALL T_GST94 ( STATUS )
      CALL T_H2FK5 ( STATUS )
      CALL T_HD2AE ( STATUS )
      CALL T_HD2PA ( STATUS )
      CALL T_HFK5Z ( STATUS )
      CALL T_ICRS2G ( STATUS )
      CALL T_IR ( STATUS )
      CALL T_JD2CAL ( STATUS )
      CALL T_JDCALF ( STATUS )
      CALL T_LD ( STATUS )
      CALL T_LDN ( STATUS )
      CALL T_LDSUN ( STATUS )
      CALL T_LTECEQ ( STATUS )
      CALL T_LTECM ( STATUS )
      CALL T_LTEQEC ( STATUS )
      CALL T_LTP ( STATUS )
      CALL T_LTPB ( STATUS )
      CALL T_LTPECL ( STATUS )
      CALL T_LTPEQU ( STATUS )
      CALL T_NUM00A ( STATUS )
      CALL T_NUM00B ( STATUS )
      CALL T_NUM06A ( STATUS )
      CALL T_NUMAT ( STATUS )
      CALL T_NUT00A ( STATUS )
      CALL T_NUT00B (STATUS )
      CALL T_NUT06A ( STATUS )
      CALL T_NUT80 ( STATUS )
      CALL T_NUTM80 ( STATUS )
      CALL T_OBL06 ( STATUS )
      CALL T_OBL80 ( STATUS )
      CALL T_P06E ( STATUS )
      CALL T_P2PV ( STATUS )
      CALL T_P2S ( STATUS )
      CALL T_PAP ( STATUS )
      CALL T_PAS ( STATUS )
      CALL T_PB06 ( STATUS )
      CALL T_PDP ( STATUS )
      CALL T_PFW06 ( STATUS )
      CALL T_PLAN94 ( STATUS )
      CALL T_PM ( STATUS )
      CALL T_PMAT00 ( STATUS )
      CALL T_PMAT06 ( STATUS )
      CALL T_PMAT76 ( STATUS )
      CALL T_PMP ( STATUS )
      CALL T_PMPX ( STATUS )
      CALL T_PMSAFE ( STATUS )
      CALL T_PN ( STATUS )
      CALL T_PN00 ( STATUS )
      CALL T_PN00A ( STATUS )
      CALL T_PN00B ( STATUS )
      CALL T_PN06A ( STATUS )
      CALL T_PN06 ( STATUS )
      CALL T_PNM00A ( STATUS )
      CALL T_PNM00B ( STATUS )
      CALL T_PNM06A ( STATUS )
      CALL T_PNM80 ( STATUS )
      CALL T_POM00 ( STATUS )
      CALL T_PPP ( STATUS )
      CALL T_PPSP ( STATUS )
      CALL T_PR00 ( STATUS )
      CALL T_PREC76 ( STATUS )
      CALL T_PV2P ( STATUS )
      CALL T_PV2S ( STATUS )
      CALL T_PVDPV ( STATUS )
      CALL T_PVM ( STATUS )
      CALL T_PVMPV ( STATUS )
      CALL T_PVPPV ( STATUS )
      CALL T_PVSTAR ( STATUS )
      CALL T_PVTOB ( STATUS )
      CALL T_PVU ( STATUS )
      CALL T_PVUP ( STATUS )
      CALL T_PVXPV ( STATUS )
      CALL T_PXP ( STATUS )
      CALL T_REFCO ( STATUS )
      CALL T_RM2V ( STATUS )
      CALL T_RV2M ( STATUS )
      CALL T_RX ( STATUS )
      CALL T_RXP ( STATUS )
      CALL T_RXPV ( STATUS )
      CALL T_RXR ( STATUS )
      CALL T_RY ( STATUS )
      CALL T_RZ ( STATUS )
      CALL T_S00A ( STATUS )
      CALL T_S00B ( STATUS )
      CALL T_S00 ( STATUS )
      CALL T_S06A ( STATUS )
      CALL T_S06 ( STATUS )
      CALL T_S2C ( STATUS )
      CALL T_S2P ( STATUS )
      CALL T_S2PV ( STATUS )
      CALL T_S2XPV ( STATUS )
      CALL T_SEPP ( STATUS )
      CALL T_SEPS ( STATUS )
      CALL T_SP00 ( STATUS )
      CALL T_STARPM ( STATUS )
      CALL T_STARPV ( STATUS )
      CALL T_SXP ( STATUS )
      CALL T_SXPV ( STATUS )
      CALL T_TAITT ( STATUS )
      CALL T_TAIUT1 ( STATUS )
      CALL T_TAIUTC ( STATUS )
      CALL T_TCBTDB ( STATUS )
      CALL T_TCGTT ( STATUS )
      CALL T_TDBTCB ( STATUS )
      CALL T_TDBTT ( STATUS )
      CALL T_TF2A ( STATUS )
      CALL T_TF2D ( STATUS )
      CALL T_TPORS ( STATUS )
      CALL T_TPORV ( STATUS )
      CALL T_TPSTS ( STATUS )
      CALL T_TPSTV ( STATUS )
      CALL T_TPXES ( STATUS )
      CALL T_TPXEV ( STATUS )
      CALL T_TR ( STATUS )
      CALL T_TRXP ( STATUS )
      CALL T_TRXPV ( STATUS )
      CALL T_TTTAI ( STATUS )
      CALL T_TTTCG ( STATUS )
      CALL T_TTTDB ( STATUS )
      CALL T_TTUT1 ( STATUS )
      CALL T_UT1TAI ( STATUS )
      CALL T_UT1TT ( STATUS )
      CALL T_UT1UTC ( STATUS )
      CALL T_UTCTAI ( STATUS )
      CALL T_UTCUT1 ( STATUS )
      CALL T_XY06 ( STATUS )
      CALL T_XYS00A ( STATUS )
      CALL T_XYS00B ( STATUS )
      CALL T_XYS06A ( STATUS )
      CALL T_ZP ( STATUS )
      CALL T_ZPV ( STATUS )
      CALL T_ZR ( STATUS )

!  Report any errors and set up an appropriate exit status:  0 on
!  success, 1 on any error -- Unix-style.  The EXIT intrinsic is
!  non-standard but common (which is portable enough for a
!  regression test).

      IF ( STATUS ) THEN
         WRITE (*,'(1X,''T_SOFA_F validation successful'')')
         JESTAT = 0
      ELSE
         WRITE (*,'(1X,''T_SOFA_F validation failed!'')')
         JESTAT = 1
      END IF

      CALL EXIT(JESTAT)

      contains

      SUBROUTINE VIV ( IVAL, IVALOK, FUNC, TEST, STATUS )
!+
!  - - - -
!   V I V
!  - - - -
!
!  Validate an integer result.
!
!  Internal routine used by T_SOFA_F program.
!
!  Given:
!     IVAL     INTEGER      value computed by routine under test
!     IVALOK   INTEGER      correct value
!     FUNC     CHARACTER    name of routine under test
!     TEST     CHARACTER    name of individual test (or ' ')
!
!  Given and returned:
!     STATUS   LOGICAL      set to .FALSE. if test fails
!
!  Called:  ERR
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      INTEGER IVAL, IVALOK
      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      IF ( IVAL .NE. IVALOK ) THEN
         CALL ERR ( FUNC, TEST, STATUS )
         WRITE (*,'(1X,''  expected ='',I10)') IVALOK
         WRITE (*,'(1X,''  actual =  '',I10)') IVAL
      END IF

      END

      SUBROUTINE VVD ( VAL, VALOK, DVAL, FUNC, TEST, STATUS )
!+
!  - - - -
!   V V D
!  - - - -
!
!  Validate a double result.
!
!  Internal routine used by T_SOFA_F program.
!
!  Given:
!     VAL      DOUBLE       value computed by routine under test
!     VALOK    DOUBLE       correct value
!     DVAL     DOUBLE       maximum allowable error
!     FUNC     CHARACTER    name of routine under test
!     TEST     CHARACTER    name of individual test (or ' ')
!
!  Given and returned:
!     STATUS   LOGICAL      set to .FALSE. if test fails
!
!  Called:  ERR
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      DOUBLE PRECISION VAL, VALOK, DVAL
      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      IF ( DABS ( VAL - VALOK ) .GT. DVAL ) THEN
         CALL ERR ( FUNC, TEST, STATUS )
         WRITE (*,'(1X,''  expected ='',G30.19)') VALOK
         WRITE (*,'(1X,''  actual =  '',G30.19)') VAL
      END IF

      END

      SUBROUTINE ERR ( FUNC, TEST, STATUS )
!+
!  - - - -
!   E R R
!  - - - -
!
!  Report a failed test.
!
!  Internal routine used by T_SOFA_F program.
!
!  Given:
!     FUNC     CHARACTER    name of routine under test
!     TEST     CHARACTER    name of individual test (or ' ')
!
!  Given and returned:
!     STATUS   LOGICAL      set to .FALSE.
!
!  This revision:  2008 November 29
!-
      IMPLICIT NONE

      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      WRITE (*,'(1X,A,'' test '',A,'' fails:'')') FUNC, TEST
      STATUS = .FALSE.

      END

      SUBROUTINE T_A2AF ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A 2 A F
!  - - - - - - - - - - -
!
!  Test A2AF routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  A2AF, VIV
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IDMSF(4)
      CHARACTER S


      CALL A2AF ( 4, 2.345D0, S, IDMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '+' ), 'A2AF', 'S', STATUS )
      CALL VIV ( IDMSF(1),  134, 'A2AF', '1', STATUS )
      CALL VIV ( IDMSF(2),   21, 'A2AF', '2', STATUS )
      CALL VIV ( IDMSF(3),   30, 'A2AF', '3', STATUS )
      CALL VIV ( IDMSF(4), 9706, 'A2AF', '4', STATUS )

      END

      SUBROUTINE T_A2TF ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A 2 T F
!  - - - - - - - - - - -
!
!  Test A2TF routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  A2TF, VIV
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IHMSF(4)
      CHARACTER S


      CALL A2TF ( 4, -3.01234D0, S, IHMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '-' ), 'A2TF', 'S', STATUS )
      CALL VIV ( IHMSF(1),   11, 'A2TF', '1', STATUS )
      CALL VIV ( IHMSF(2),   30, 'A2TF', '2', STATUS )
      CALL VIV ( IHMSF(3),   22, 'A2TF', '3', STATUS )
      CALL VIV ( IHMSF(4), 6484, 'A2TF', '4', STATUS )

      END

      SUBROUTINE T_AB ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ A B
!  - - - - - - - - -
!
!  Test AB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  AB, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PNAT(3), V(3), S, BM1, PPR(3)


      PNAT(1) = -0.76321968546737951D0
      PNAT(2) = -0.60869453983060384D0
      PNAT(3) = -0.21676408580639883D0
      V(1) =  2.1044018893653786D-5
      V(2) = -8.9108923304429319D-5
      V(3) = -3.8633714797716569D-5
      S = 0.99980921395708788D0
      BM1 = 0.99999999506209258D0

      CALL AB ( PNAT, V, S, BM1, PPR )

      CALL VVD ( PPR(1), -0.7631631094219556269D0, 1D-12,&
                 'AB', '1', STATUS )
      CALL VVD ( PPR(2), -0.6087553082505590832D0, 1D-12,&
                 'AB', '2', STATUS )
      CALL VVD ( PPR(3), -0.2167926269368471279D0, 1D-12,&
                 'AB', '3', STATUS )

      END

      SUBROUTINE T_AE2HD ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ A E 2 H D
!  - - - - - - - - - - - -
!
!  Test AE2HD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  AE2HD, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A, E, P, H, D


      A = 5.5D0
      E = 1.1D0
      P = 0.7D0

      CALL AE2HD ( A, E, P, H, D )

      CALL VVD ( H, 0.5933291115507309663D0, 1D-14,&
                 'AE2HD', 'H', STATUS )
      CALL VVD ( D, 0.9613934761647817620D0, 1D-14,&
                 'AE2HD', 'D', STATUS )

      END

      SUBROUTINE T_AF2A ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A F 2 A
!  - - - - - - - - - - -
!
!  Test AF2A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  AF2A, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A
      INTEGER J


      CALL AF2A ( '-', 45, 13, 27.2D0, A, J )

      CALL VVD ( A, -0.7893115794313644842D0, 1D-12,&
                 'AF2A', 'A', STATUS )
      CALL VIV ( J, 0, 'AF2A', 'J', STATUS )

      END

      SUBROUTINE T_ANP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ A N P
!  - - - - - - - - - -
!
!  Test ANP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ANP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( ANP ( -0.1D0 ), 6.183185307179586477D0, 1D-12,&
                 'ANP', ' ', STATUS )

      END

      SUBROUTINE T_ANPM ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A N P M
!  - - - - - - - - - - -
!
!  Test ANPM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ANPM, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( ANPM ( -4D0 ), 2.283185307179586477D0, 1D-12,&
                 'ANPM', ' ', STATUS )

      END

      SUBROUTINE T_APCG ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P C G
!  - - - - - - - - - - -
!
!  Test APCG routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCG, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, EBPV(3,2), EHP(3), ASTROM(30)

      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      EBPV(1,1) =  0.901310875D0
      EBPV(2,1) = -0.417402664D0
      EBPV(3,1) = -0.180982288D0
      EBPV(1,2) =  0.00742727954D0
      EBPV(2,2) =  0.0140507459D0
      EBPV(3,2) =  0.00609045792D0
      EHP(1) =  0.903358544D0
      EHP(2) = -0.415395237D0
      EHP(3) = -0.180084014D0

      CALL APCG ( DATE1, DATE2, EBPV, EHP, ASTROM )

      CALL VVD ( ASTROM(1), 12.65133794027378508D0, 1D-11,&
                 'APCG', '1', STATUS )
      CALL VVD ( ASTROM(2), 0.901310875D0, 1D-12,&
                 'APCG', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.417402664D0, 1D-12,&
                 'APCG', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.180982288D0, 1D-12,&
                 'APCG', '4', STATUS )
      CALL VVD ( ASTROM(5), 0.8940025429324143045D0, 1D-12,&
                 'APCG', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.4110930268679817955D0, 1D-12,&
                 'APCG', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.1782189004872870264D0, 1D-12,&
                 'APCG', '7', STATUS )
      CALL VVD ( ASTROM(8), 1.010465295811013146D0, 1D-12,&
                 'APCG', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.4289638913597693554D-4, 1D-16,&
                 'APCG', '9', STATUS )
      CALL VVD ( ASTROM(10), 0.8115034051581320575D-4, 1D-16,&
                 'APCG', '10', STATUS )
      CALL VVD ( ASTROM(11), 0.3517555136380563427D-4, 1D-16,&
                 'APCG', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999951686012981D0, 1D-12,&
                 'APCG', '12', STATUS )
      CALL VVD ( ASTROM(13), 1D0, 0D0,&
                 'APCG', '13', STATUS )
      CALL VVD ( ASTROM(14), 0D0, 0D0,&
                 'APCG', '14', STATUS )
      CALL VVD ( ASTROM(15), 0D0, 0D0,&
                 'APCG', '15', STATUS )
      CALL VVD ( ASTROM(16), 0D0, 0D0,&
                 'APCG', '16', STATUS )
      CALL VVD ( ASTROM(17), 1D0, 0D0,&
                 'APCG', '17', STATUS )
      CALL VVD ( ASTROM(18), 0D0, 0D0,&
                 'APCG', '18', STATUS )
      CALL VVD ( ASTROM(19), 0D0, 0D0,&
                 'APCG', '19', STATUS )
      CALL VVD ( ASTROM(20), 0D0, 0D0,&
                 'APCG', '20', STATUS )
      CALL VVD ( ASTROM(21), 1D0, 0D0,&
                 'APCG', '21', STATUS )

      END

      SUBROUTINE T_APCG13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P C G 1 3
!  - - - - - - - - - - - - -
!
!  Test APCG13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCG13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30)

      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0

      CALL APCG13 ( DATE1, DATE2, ASTROM )

      CALL VVD ( ASTROM(1), 12.65133794027378508D0, 1D-11,&
                 'APCG13', '1', STATUS )
      CALL VVD ( ASTROM(2), 0.9013108747340644755D0, 1D-12,&
                 'APCG13', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.4174026640406119957D0, 1D-12,&
                 'APCG13', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.1809822877867817771D0, 1D-12,&
                 'APCG13', '4', STATUS )
      CALL VVD ( ASTROM(5), 0.8940025429255499549D0, 1D-12,&
                 'APCG13', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.4110930268331896318D0, 1D-12,&
                 'APCG13', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.1782189006019749850D0, 1D-12,&
                 'APCG13', '7', STATUS )
      CALL VVD ( ASTROM(8), 1.010465295964664178D0, 1D-12,&
                 'APCG13', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.4289638912941341125D-4, 1D-16,&
                 'APCG13', '9', STATUS )
      CALL VVD ( ASTROM(10), 0.8115034032405042132D-4, 1D-16,&
                 'APCG13', '10', STATUS )
      CALL VVD ( ASTROM(11), 0.3517555135536470279D-4, 1D-16,&
                 'APCG13', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999951686013142D0, 1D-12,&
                 'APCG13', '12', STATUS )
      CALL VVD ( ASTROM(13), 1D0, 0D0,&
                 'APCG13', '13', STATUS )
      CALL VVD ( ASTROM(14), 0D0, 0D0,&
                 'APCG13', '14', STATUS )
      CALL VVD ( ASTROM(15), 0D0, 0D0,&
                 'APCG13', '15', STATUS )
      CALL VVD ( ASTROM(16), 0D0, 0D0,&
                 'APCG13', '16', STATUS )
      CALL VVD ( ASTROM(17), 1D0, 0D0,&
                 'APCG13', '17', STATUS )
      CALL VVD ( ASTROM(18), 0D0, 0D0,&
                 'APCG13', '18', STATUS )
      CALL VVD ( ASTROM(19), 0D0, 0D0,&
                 'APCG13', '19', STATUS )
      CALL VVD ( ASTROM(20), 0D0, 0D0,&
                 'APCG13', '20', STATUS )
      CALL VVD ( ASTROM(21), 1D0, 0D0,&
                 'APCG13', '21', STATUS )

      END

      SUBROUTINE T_APCI ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P C I
!  - - - - - - - - - - -
!
!  Test APCI routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, EBPV(3,2), EHP(3), X, Y, S,&
                       ASTROM(30)

      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      EBPV(1,1) =  0.901310875D0
      EBPV(2,1) = -0.417402664D0
      EBPV(3,1) = -0.180982288D0
      EBPV(1,2) =  0.00742727954D0
      EBPV(2,2) =  0.0140507459D0
      EBPV(3,2) =  0.00609045792D0
      EHP(1) =  0.903358544D0
      EHP(2) = -0.415395237D0
      EHP(3) = -0.180084014D0
      X =  0.0013122272D0
      Y = -2.92808623D-5
      S =  3.05749468D-8

      CALL APCI ( DATE1, DATE2, EBPV, EHP, X, Y, S, ASTROM )

      CALL VVD ( ASTROM(1), 12.65133794027378508D0, 1D-11,&
                 'APCI', '1', STATUS )
      CALL VVD ( ASTROM(2), 0.901310875D0, 1D-12,&
                 'APCI', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.417402664D0, 1D-12,&
                 'APCI', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.180982288D0, 1D-12,&
                 'APCI', '4', STATUS )
      CALL VVD ( ASTROM(5), 0.8940025429324143045D0, 1D-12,&
                 'APCI', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.4110930268679817955D0, 1D-12,&
                 'APCI', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.1782189004872870264D0, 1D-12,&
                 'APCI', '7', STATUS )
      CALL VVD ( ASTROM(8), 1.010465295811013146D0, 1D-12,&
                 'APCI', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.4289638913597693554D-4, 1D-16,&
                 'APCI', '9', STATUS )
      CALL VVD ( ASTROM(10), 0.8115034051581320575D-4, 1D-16,&
                 'APCI', '10', STATUS )
      CALL VVD ( ASTROM(11), 0.3517555136380563427D-4, 1D-16,&
                 'APCI', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999951686012981D0, 1D-12,&
                 'APCI', '12', STATUS )
      CALL VVD ( ASTROM(13), 0.9999991390295159156D0, 1D-12,&
                 'APCI', '13', STATUS )
      CALL VVD ( ASTROM(14), 0.4978650072505016932D-7, 1D-12,&
                 'APCI', '14', STATUS )
      CALL VVD ( ASTROM(15), 0.1312227200000000000D-2, 1D-12,&
                 'APCI', '15', STATUS )
      CALL VVD ( ASTROM(16), -0.1136336653771609630D-7, 1D-12,&
                 'APCI', '16', STATUS )
      CALL VVD ( ASTROM(17), 0.9999999995713154868D0, 1D-12,&
                 'APCI', '17', STATUS )
      CALL VVD ( ASTROM(18), -0.2928086230000000000D-4, 1D-12,&
                 'APCI', '18', STATUS )
      CALL VVD ( ASTROM(19), -0.1312227200895260194D-2, 1D-12,&
                 'APCI', '19', STATUS )
      CALL VVD ( ASTROM(20), 0.2928082217872315680D-4, 1D-12,&
                 'APCI', '20', STATUS )
      CALL VVD ( ASTROM(21), 0.9999991386008323373D0, 1D-12,&
                 'APCI', '21', STATUS )

      END

      SUBROUTINE T_APCI13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P C I 1 3
!  - - - - - - - - - - - - -
!
!  Test APCI13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO

      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0

      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )

      CALL VVD ( ASTROM(1), 12.65133794027378508D0, 1D-11,&
                 'APCI13', '1', STATUS )
      CALL VVD ( ASTROM(2), 0.9013108747340644755D0, 1D-12,&
                 'APCI13', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.4174026640406119957D0, 1D-12,&
                 'APCI13', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.1809822877867817771D0, 1D-12,&
                 'APCI13', '4', STATUS )
      CALL VVD ( ASTROM(5), 0.8940025429255499549D0, 1D-12,&
                 'APCI13', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.4110930268331896318D0, 1D-12,&
                 'APCI13', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.1782189006019749850D0, 1D-12,&
                 'APCI13', '7', STATUS )
      CALL VVD ( ASTROM(8), 1.010465295964664178D0, 1D-12,&
                 'APCI13', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.4289638912941341125D-4, 1D-16,&
                 'APCI13', '9', STATUS )
      CALL VVD ( ASTROM(10), 0.8115034032405042132D-4, 1D-16,&
                 'APCI13', '10', STATUS )
      CALL VVD ( ASTROM(11), 0.3517555135536470279D-4, 1D-16,&
                 'APCI13', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999951686013142D0, 1D-12,&
                 'APCI13', '12', STATUS )
      CALL VVD ( ASTROM(13), 0.9999992060376761710D0, 1D-12,&
                 'APCI13', '13', STATUS )
      CALL VVD ( ASTROM(14), 0.4124244860106037157D-7, 1D-12,&
                 'APCI13', '14', STATUS )
      CALL VVD ( ASTROM(15), 0.1260128571051709670D-2, 1D-12,&
                 'APCI13', '15', STATUS )
      CALL VVD ( ASTROM(16), -0.1282291987222130690D-7, 1D-12,&
                 'APCI13', '16', STATUS )
      CALL VVD ( ASTROM(17), 0.9999999997456835325D0, 1D-12,&
                 'APCI13', '17', STATUS )
      CALL VVD ( ASTROM(18), -0.2255288829420524935D-4, 1D-12,&
                 'APCI13', '18', STATUS )
      CALL VVD ( ASTROM(19), -0.1260128571661374559D-2, 1D-12,&
                 'APCI13', '19', STATUS )
      CALL VVD ( ASTROM(20), 0.2255285422953395494D-4, 1D-12,&
                 'APCI13', '20', STATUS )
      CALL VVD ( ASTROM(21), 0.9999992057833604343D0, 1D-12,&
                 'APCI13', '21', STATUS )
      CALL VVD ( EO, -0.2900618712657375647D-2, 1D-12,&
                 'APCI13', 'EO', STATUS )

      END

      SUBROUTINE T_APCO ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P C O
!  - - - - - - - - - - -
!
!  Test APCO routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCO, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS
      DOUBLE PRECISION DATE1, DATE2, EBPV(3,2), EHP(3), X, Y, S,&
                       THETA, ELONG, PHI, HM, XP, YP, SP, REFA, REFB,&
                       ASTROM(30)


      DATE1 = 2456384.5D0
      DATE2 = 0.970031644D0
      EBPV(1,1) = -0.974170438D0
      EBPV(2,1) = -0.211520082D0
      EBPV(3,1) = -0.0917583024D0
      EBPV(1,2) = 0.00364365824D0
      EBPV(2,2) = -0.0154287319D0
      EBPV(3,2) = -0.00668922024D0
      EHP(1) = -0.973458265D0
      EHP(2) = -0.209215307D0
      EHP(3) = -0.0906996477D0
      X = 0.0013122272D0
      Y = -2.92808623D-5
      S = 3.05749468D-8
      THETA = 3.14540971D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      SP = -3.01974337D-11
      REFA  = 0.000201418779D0
      REFB  = -2.36140831D-7

      CALL APCO ( DATE1, DATE2, EBPV, EHP, X, Y, S,&
                      THETA, ELONG, PHI, HM, XP, YP, SP,&
                      REFA, REFB, ASTROM )

      CALL VVD ( ASTROM(1), 13.25248468622587269D0, 1D-11,&
                 'APCO', '1', STATUS )
      CALL VVD ( ASTROM(2), -0.9741827110630322720D0, 1D-12,&
                 'APCO', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.2115130190135344832D0, 1D-12,&
                 'APCO', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.09179840186949532298D0, 1D-12,&
                 'APCO', '4', STATUS )
      CALL VVD ( ASTROM(5), -0.9736425571689739035D0, 1D-12,&
                 'APCO', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.2092452125849330936D0, 1D-12,&
                 'APCO', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.09075578152243272599D0, 1D-12,&
                 'APCO', '7', STATUS )
      CALL VVD ( ASTROM(8), 0.9998233241709957653D0, 1D-12,&
                 'APCO', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.2078704992916728762D-4, 1D-16,&
                 'APCO', '9', STATUS )
      CALL VVD ( ASTROM(10), -0.8955360107151952319D-4, 1D-16,&
                 'APCO', '10', STATUS )
      CALL VVD ( ASTROM(11), -0.3863338994288951082D-4, 1D-16,&
                 'APCO', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999950277561236D0, 1D-12,&
                 'APCO', '12', STATUS )
      CALL VVD ( ASTROM(13), 0.9999991390295159156D0, 1D-12,&
                 'APCO', '13', STATUS )
      CALL VVD ( ASTROM(14), 0.4978650072505016932D-7, 1D-12,&
                 'APCO', '14', STATUS )
      CALL VVD ( ASTROM(15), 0.1312227200000000000D-2, 1D-12,&
                 'APCO', '15', STATUS )
      CALL VVD ( ASTROM(16), -0.1136336653771609630D-7, 1D-12,&
                 'APCO', '16', STATUS )
      CALL VVD ( ASTROM(17), 0.9999999995713154868D0, 1D-12,&
                 'APCO', '17', STATUS )
      CALL VVD ( ASTROM(18), -0.2928086230000000000D-4, 1D-12,&
                 'APCO', '18', STATUS )
      CALL VVD ( ASTROM(19), -0.1312227200895260194D-2, 1D-12,&
                 'APCO', '19', STATUS )
      CALL VVD ( ASTROM(20), 0.2928082217872315680D-4, 1D-12,&
                 'APCO', '20', STATUS )
      CALL VVD ( ASTROM(21), 0.9999991386008323373D0, 1D-12,&
                 'APCO', '21', STATUS )
      CALL VVD ( ASTROM(22), -0.5278008060301974337D0, 1D-12,&
                 'APCO', '22', STATUS )
      CALL VVD ( ASTROM(23), 0.1133427418174939329D-5, 1D-17,&
                 'APCO', '23', STATUS )
      CALL VVD ( ASTROM(24), 0.1453347595745898629D-5, 1D-17,&
                 'APCO', '24', STATUS )
      CALL VVD ( ASTROM(25), -0.9440115679003211329D0, 1D-12,&
                 'APCO', '25', STATUS )
      CALL VVD ( ASTROM(26), 0.3299123514971474711D0, 1D-12,&
                 'APCO', '26', STATUS )
      CALL VVD ( ASTROM(27), 0D0, 0D0,&
                 'APCO', '27', STATUS )
      CALL VVD ( ASTROM(28), 2.617608903969802566D0, 1D-12,&
                 'APCO', '28', STATUS )
      CALL VVD ( ASTROM(29), 0.2014187790000000000D-3, 1D-15,&
                 'APCO', '29', STATUS )
      CALL VVD ( ASTROM(30), -0.2361408310000000000D-6, 1D-18,&
                 'APCO', '30', STATUS )

      END

      SUBROUTINE T_APCO13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P C O 1 3
!  - - - - - - - - - - - - -
!
!  Test APCO13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCO13, VIV, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                       PHPA, TC, RH, WL, ASTROM(30), EO
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      CALL APCO13 ( UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                        PHPA, TC, RH, WL, ASTROM, EO, J )

      CALL VIV ( J, 0, 'APCO13', 'J', STATUS )
      CALL VVD ( ASTROM(1), 13.25248468622475727D0, 1D-11,&
                 'APCO13', '1', STATUS )
      CALL VVD ( ASTROM(2), -0.9741827107320875162D0, 1D-12,&
                 'APCO13', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.2115130190489716682D0, 1D-12,&
                 'APCO13', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.09179840189496755339D0, 1D-12,&
                 'APCO13', '4', STATUS )
      CALL VVD ( ASTROM(5), -0.9736425572586935247D0, 1D-12,&
                 'APCO13', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.2092452121603336166D0, 1D-12,&
                 'APCO13', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.09075578153885665295D0, 1D-12,&
                 'APCO13', '7', STATUS )
      CALL VVD ( ASTROM(8), 0.9998233240913898141D0, 1D-12,&
                 'APCO13', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.2078704994520489246D-4, 1D-16,&
                 'APCO13', '9', STATUS )
      CALL VVD ( ASTROM(10), -0.8955360133238868938D-4, 1D-16,&
                 'APCO13', '10', STATUS )
      CALL VVD ( ASTROM(11), -0.3863338993055887398D-4, 1D-16,&
                 'APCO13', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999950277561004D0, 1D-12,&
                 'APCO13', '12', STATUS )
      CALL VVD ( ASTROM(13), 0.9999991390295147999D0, 1D-12,&
                 'APCO13', '13', STATUS )
      CALL VVD ( ASTROM(14), 0.4978650075315529277D-7, 1D-12,&
                 'APCO13', '14', STATUS )
      CALL VVD ( ASTROM(15), 0.001312227200850293372D0, 1D-12,&
                 'APCO13', '15', STATUS )
      CALL VVD ( ASTROM(16), -0.1136336652812486604D-7, 1D-12,&
                 'APCO13', '16', STATUS )
      CALL VVD ( ASTROM(17), 0.9999999995713154865D0, 1D-12,&
                 'APCO13', '17', STATUS )
      CALL VVD ( ASTROM(18), -0.2928086230975367296D-4, 1D-12,&
                 'APCO13', '18', STATUS )
      CALL VVD ( ASTROM(19), -0.001312227201745553566D0, 1D-12,&
                 'APCO13', '19', STATUS )
      CALL VVD ( ASTROM(20), 0.2928082218847679162D-4, 1D-12,&
                 'APCO13', '20', STATUS )
      CALL VVD ( ASTROM(21), 0.9999991386008312212D0, 1D-12,&
                 'APCO13', '21', STATUS )
      CALL VVD ( ASTROM(22), -0.5278008060301974337D0, 1D-12,&
                 'APCO13', '22', STATUS )
      CALL VVD ( ASTROM(23), 0.1133427418174939329D-5, 1D-17,&
                 'APCO13', '23', STATUS )
      CALL VVD ( ASTROM(24), 0.1453347595745898629D-5, 1D-17,&
                 'APCO13', '24', STATUS )
      CALL VVD ( ASTROM(25), -0.9440115679003211329D0, 1D-12,&
                 'APCO13', '25', STATUS )
      CALL VVD ( ASTROM(26), 0.3299123514971474711D0, 1D-12,&
                 'APCO13', '26', STATUS )
      CALL VVD ( ASTROM(27), 0D0, 0D0,&
                 'APCO13', '27', STATUS )
      CALL VVD ( ASTROM(28), 2.617608909189066140D0, 1D-12,&
                 'APCO13', '28', STATUS )
      CALL VVD ( ASTROM(29), 0.2014187785940396921D-3, 1D-15,&
                 'APCO13', '29', STATUS )
      CALL VVD ( ASTROM(30), -0.2361408314943696227D-6, 1D-18,&
                 'APCO13', '30', STATUS )
      CALL VVD ( EO, -0.003020548354802412839D0, 1D-14,&
                 'APCO13', 'EO', STATUS )

      END

      SUBROUTINE T_APCS ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P C S
!  - - - - - - - - - - -
!
!  Test APCS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCS, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, PV(3,2), EBPV(3,2), EHP(3),&
                       ASTROM(30)

      DATE1 = 2456384.5D0
      DATE2 = 0.970031644D0
      PV(1,1) = -1836024.09D0
      PV(2,1) = 1056607.72D0
      PV(3,1) = -5998795.26D0
      PV(1,2) = -77.0361767D0
      PV(2,2) = -133.310856D0
      PV(3,2) = 0.0971855934D0
      EBPV(1,1) = -0.974170438D0
      EBPV(2,1) = -0.211520082D0
      EBPV(3,1) = -0.0917583024D0
      EBPV(1,2) = 0.00364365824D0
      EBPV(2,2) = -0.0154287319D0
      EBPV(3,2) = -0.00668922024D0
      EHP(1) = -0.973458265D0
      EHP(2) = -0.209215307D0
      EHP(3) = -0.0906996477D0

      CALL APCS ( DATE1, DATE2, PV, EBPV, EHP, ASTROM )

      CALL VVD ( ASTROM(1), 13.25248468622587269D0, 1D-11,&
                 'APCS', '1', STATUS )
      CALL VVD ( ASTROM(2), -0.9741827110629881886D0, 1D-12,&
                 'APCS', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.2115130190136415986D0, 1D-12,&
                 'APCS', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.09179840186954412099D0, 1D-12,&
                 'APCS', '4', STATUS )
      CALL VVD ( ASTROM(5), -0.9736425571689454706D0, 1D-12,&
                 'APCS', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.2092452125850435930D0, 1D-12,&
                 'APCS', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.09075578152248299218D0, 1D-12,&
                 'APCS', '7', STATUS )
      CALL VVD ( ASTROM(8), 0.9998233241709796859D0, 1D-12,&
                 'APCS', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.2078704993282685510D-4, 1D-16,&
                 'APCS', '9', STATUS )
      CALL VVD ( ASTROM(10), -0.8955360106989405683D-4, 1D-16,&
                 'APCS', '10', STATUS )
      CALL VVD ( ASTROM(11), -0.3863338994289409097D-4, 1D-16,&
                 'APCS', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999950277561237D0, 1D-12,&
                 'APCS', '12', STATUS )
      CALL VVD ( ASTROM(13), 1D0, 0D0,&
                 'APCS', '13', STATUS )
      CALL VVD ( ASTROM(14), 0D0, 0D0,&
                 'APCS', '14', STATUS )
      CALL VVD ( ASTROM(15), 0D0, 0D0,&
                 'APCS', '15', STATUS )
      CALL VVD ( ASTROM(16), 0D0, 0D0,&
                 'APCS', '16', STATUS )
      CALL VVD ( ASTROM(17), 1D0, 0D0,&
                 'APCS', '17', STATUS )
      CALL VVD ( ASTROM(18), 0D0, 0D0,&
                 'APCS', '18', STATUS )
      CALL VVD ( ASTROM(19), 0D0, 0D0,&
                 'APCS', '19', STATUS )
      CALL VVD ( ASTROM(20), 0D0, 0D0,&
                 'APCS', '20', STATUS )
      CALL VVD ( ASTROM(21), 1D0, 0D0,&
                 'APCS', '21', STATUS )

      END

      SUBROUTINE T_APCS13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P C S 1 3
!  - - - - - - - - - - - - -
!
!  Test APCS13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCS13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, PV(3,2), ASTROM(30)


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      PV(1,1) = -6241497.16D0
      PV(2,1) = 401346.896D0
      PV(3,1) = -1251136.04D0
      PV(1,2) = -29.264597D0
      PV(2,2) = -455.021831D0
      PV(3,2) = 0.0266151194D0

      CALL APCS13 ( DATE1, DATE2, PV, ASTROM )

      CALL VVD ( ASTROM(1), 12.65133794027378508D0, 1D-11,&
                 'APCS13', '1', STATUS )
      CALL VVD ( ASTROM(2), 0.9012691529025250644D0, 1D-12,&
                 'APCS13', '2', STATUS )
      CALL VVD ( ASTROM(3), -0.4173999812023194317D0, 1D-12,&
                 'APCS13', '3', STATUS )
      CALL VVD ( ASTROM(4), -0.1809906511146429670D0, 1D-12,&
                 'APCS13', '4', STATUS )
      CALL VVD ( ASTROM(5), 0.8939939101760130792D0, 1D-12,&
                 'APCS13', '5', STATUS )
      CALL VVD ( ASTROM(6), -0.4111053891734021478D0, 1D-12,&
                 'APCS13', '6', STATUS )
      CALL VVD ( ASTROM(7), -0.1782336880636997374D0, 1D-12,&
                 'APCS13', '7', STATUS )
      CALL VVD ( ASTROM(8), 1.010428384373491095D0, 1D-12,&
                 'APCS13', '8', STATUS )
      CALL VVD ( ASTROM(9), 0.4279877294121697570D-4, 1D-16,&
                 'APCS13', '9', STATUS )
      CALL VVD ( ASTROM(10), 0.7963255087052120678D-4, 1D-16,&
                 'APCS13', '10', STATUS )
      CALL VVD ( ASTROM(11), 0.3517564013384691531D-4, 1D-16,&
                 'APCS13', '11', STATUS )
      CALL VVD ( ASTROM(12), 0.9999999952947980978D0, 1D-12,&
                 'APCS13', '12', STATUS )
      CALL VVD ( ASTROM(13), 1D0, 0D0,&
                 'APCS13', '13', STATUS )
      CALL VVD ( ASTROM(14), 0D0, 0D0,&
                 'APCS13', '14', STATUS )
      CALL VVD ( ASTROM(15), 0D0, 0D0,&
                 'APCS13', '15', STATUS )
      CALL VVD ( ASTROM(16), 0D0, 0D0,&
                 'APCS13', '16', STATUS )
      CALL VVD ( ASTROM(17), 1D0, 0D0,&
                 'APCS13', '17', STATUS )
      CALL VVD ( ASTROM(18), 0D0, 0D0,&
                 'APCS13', '18', STATUS )
      CALL VVD ( ASTROM(19), 0D0, 0D0,&
                 'APCS13', '19', STATUS )
      CALL VVD ( ASTROM(20), 0D0, 0D0,&
                 'APCS13', '20', STATUS )
      CALL VVD ( ASTROM(21), 1D0, 0D0,&
                 'APCS13', '21', STATUS )

      END

      SUBROUTINE T_APER ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P E R
!  - - - - - - - - - - -
!
!  Test APER routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APER, VVD
!
!  This revision:  2013 September 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION ASTROM(30), THETA


      ASTROM(22) = 1.234D0
      THETA = 5.678D0

      CALL APER ( THETA, ASTROM )

      CALL VVD ( ASTROM(28), 6.912000000000000000D0, 1D-12,&
                 'APER', '1', STATUS )

      END

      SUBROUTINE T_APER13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P E R 1 3
!  - - - - - - - - - - - - -
!
!  Test APER13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APER13, VVD
!
!  This revision:  2013 September 25
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION ASTROM(30), UT11, UT12


      ASTROM(22) = 1.234D0
      UT11 = 2456165.5D0
      UT12 = 0.401182685D0

      CALL APER13 ( UT11, UT12, ASTROM )

      CALL VVD ( ASTROM(28), 3.316236661789694933D0, 1D-12,&
                 'APER13', '1', STATUS )

      END

      SUBROUTINE T_APIO ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ A P I O
!  - - - - - - - - - - -
!
!  Test APIO routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APIO, VVD
!
!  This revision:  2013 September 30
!-

      IMPLICIT NONE

      LOGICAL STATUS
      DOUBLE PRECISION SP, THETA, ELONG, PHI, HM, XP, YP,&
                       REFA, REFB, ASTROM(30)


      SP = -3.01974337D-11
      THETA = 3.14540971D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      REFA  = 0.000201418779D0
      REFB  = -2.36140831D-7

      CALL APIO ( SP, THETA, ELONG, PHI, HM, XP, YP,&
                      REFA, REFB, ASTROM )

      CALL VVD ( ASTROM(22), -0.5278008060301974337D0, 1D-12,&
                 'APIO', '22', STATUS )
      CALL VVD ( ASTROM(23), 0.1133427418174939329D-5, 1D-17,&
                 'APIO', '23', STATUS )
      CALL VVD ( ASTROM(24), 0.1453347595745898629D-5, 1D-17,&
                 'APIO', '24', STATUS )
      CALL VVD ( ASTROM(25), -0.9440115679003211329D0, 1D-12,&
                 'APIO', '25', STATUS )
      CALL VVD ( ASTROM(26), 0.3299123514971474711D0, 1D-12,&
                 'APIO', '26', STATUS )
      CALL VVD ( ASTROM(27), 0.5135843661699913529D-6, 1D-12,&
                 'APIO', '27', STATUS )
      CALL VVD ( ASTROM(28), 2.617608903969802566D0, 1D-12,&
                 'APIO', '28', STATUS )
      CALL VVD ( ASTROM(29), 0.2014187790000000000D-3, 1D-15,&
                 'APIO', '29', STATUS )
      CALL VVD ( ASTROM(30), -0.2361408310000000000D-6, 1D-18,&
                 'APIO', '30', STATUS )

      END

      SUBROUTINE T_APIO13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A P I O 1 3
!  - - - - - - - - - - - - -
!
!  Test APIO13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APIO13, VVD, VIV
!
!  This revision:  2013 October 2
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                       PHPA, TC, RH, WL, ASTROM(30)
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      CALL APIO13 ( UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                        PHPA, TC, RH, WL, ASTROM, J )

      CALL VVD ( ASTROM(22), -0.5278008060301974337D0, 1D-12,&
                 'APIO13', '22', STATUS )
      CALL VVD ( ASTROM(23), 0.1133427418174939329D-5, 1D-17,&
                 'APIO13', '23', STATUS )
      CALL VVD ( ASTROM(24), 0.1453347595745898629D-5, 1D-17,&
                 'APIO13', '24', STATUS )
      CALL VVD ( ASTROM(25), -0.9440115679003211329D0, 1D-12,&
                 'APIO13', '25', STATUS )
      CALL VVD ( ASTROM(26), 0.3299123514971474711D0, 1D-12,&
                 'APIO13', '26', STATUS )
      CALL VVD ( ASTROM(27), 0.5135843661699913529D-6, 1D-12,&
                 'APIO13', '27', STATUS )
      CALL VVD ( ASTROM(28), 2.617608909189066140D0, 1D-12,&
                 'APIO13', '28', STATUS )
      CALL VVD ( ASTROM(29), 0.2014187785940396921D-3, 1D-15,&
                 'APIO13', '29', STATUS )
      CALL VVD ( ASTROM(30), -0.2361408314943696227D-6, 1D-18,&
                 'APIO13', '30', STATUS )
      CALL VIV ( J, 0, 'APIO13', 'J', STATUS )

      END

      SUBROUTINE T_ATCI13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T C I 1 3
!  - - - - - - - - - - - - -
!
!  Test ATCI13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATCI13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC, DC, PR, PD, PX, RV, DATE1, DATE2,&
                       RI, DI, EO


      RC = 2.71D0
      DC = 0.174D0
      PR = 1D-5
      PD = 5D-6
      PX = 0.1D0
      RV = 55D0
      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0

      CALL ATCI13 ( RC, DC, PR, PD, PX, RV, DATE1, DATE2,&
                        RI, DI, EO )

      CALL VVD ( RI, 2.710121572968696744D0, 1D-12,&
                 'ATCI13', 'RI', STATUS )
      CALL VVD ( DI, 0.1729371367219539137D0, 1D-12,&
                 'ATCI13', 'DI', STATUS )
      CALL VVD ( EO, -0.002900618712657375647D0, 1D-14,&
                 'ATCI13', 'EO', STATUS )

      END

      SUBROUTINE T_ATCIQ ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ A T C I Q
!  - - - - - - - - - - - -
!
!  Test ATCIQ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, ATCIQ, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO,&
                       RC, DC, PR, PD, PX, RV, RI, DI


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )
      RC = 2.71D0
      DC = 0.174D0
      PR = 1D-5
      PD = 5D-6
      PX = 0.1D0
      RV = 55D0

      CALL ATCIQ ( RC, DC, PR, PD, PX, RV, ASTROM, RI, DI )

      CALL VVD ( RI, 2.710121572968696744D0, 1D-12,&
                 'ATCIQ', 'RI', STATUS )
      CALL VVD ( DI, 0.1729371367219539137D0, 1D-12,&
                 'ATCIQ', 'DI', STATUS )

      END

      SUBROUTINE T_ATCIQN ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T C I Q N
!  - - - - - - - - - - - - -
!
!  Test ATCIQN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, ATCIQN, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO,&
                       RC, DC, PR, PD, PX, RV, B(8,3), RI, DI


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )
      RC = 2.71D0
      DC = 0.174D0
      PR = 1D-5
      PD = 5D-6
      PX = 0.1D0
      RV = 55D0
      B(1,1) = 0.00028574D0
      B(2,1) = 3D-10
      B(3,1) = -7.81014427D0
      B(4,1) = -5.60956681D0
      B(5,1) = -1.98079819D0
      B(6,1) = 0.0030723249D0
      B(7,1) = -0.00406995477D0
      B(8,1) = -0.00181335842D0
      B(1,2) = 0.00095435D0
      B(2,2) = 3D-9
      B(3,2) = 0.738098796D0
      B(4,2) = 4.63658692D0
      B(5,2) = 1.9693136D0
      B(6,2) = -0.00755816922D0
      B(7,2) = 0.00126913722D0
      B(8,2) = 0.000727999001D0
      B(1,3) = 1D0
      B(2,3) = 6D-6
      B(3,3) = -0.000712174377D0
      B(4,3) = -0.00230478303D0
      B(5,3) = -0.00105865966D0
      B(6,3) = 6.29235213D-6
      B(7,3) = -3.30888387D-7
      B(8,3) = -2.96486623D-7

      CALL ATCIQN ( RC, DC, PR, PD, PX, RV, ASTROM, 3, B, RI, DI )

      CALL VVD ( RI, 2.710122008104983335D0, 1D-12,&
                 'ATCIQN', 'RI', STATUS )
      CALL VVD ( DI, 0.1729371916492767821D0, 1D-12,&
                 'ATCIQN', 'DI', STATUS )

      END

      SUBROUTINE T_ATCIQZ ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T C I Q Z
!  - - - - - - - - - - - - -
!
!  Test ATCIQZ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, ATCIQZ, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO, RC, DC, RI, DI


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )
      RC = 2.71D0
      DC = 0.174D0

      CALL ATCIQZ ( RC, DC, ASTROM, RI, DI )

      CALL VVD ( RI, 2.709994899247256984D0, 1D-12,&
                 'ATCIQZ', 'RI', STATUS )
      CALL VVD ( DI, 0.1728740720984931891D0, 1D-12,&
                 'ATCIQZ', 'DI', STATUS )

      END

      SUBROUTINE T_ATCO13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T C O 1 3
!  - - - - - - - - - - - - -
!
!  Test ATCO13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATCO13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC, DC, PR, PD, PX, RV, UTC1, UTC2, DUT1,&
                       ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                       AOB, ZOB, HOB, DOB, ROB, EO
      INTEGER J


      RC = 2.71D0
      DC = 0.174D0
      PR = 1D-5
      PD = 5D-6
      PX = 0.1D0
      RV = 55D0
      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      CALL ATCO13 ( RC, DC, PR, PD, PX, RV,&
                        UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                        PHPA, TC, RH, WL,&
                        AOB, ZOB, HOB, DOB, ROB, EO, J )

      CALL VVD ( AOB, 0.09251774485385390973D0, 1D-12,&
                 'ATCO13', 'AOB', STATUS )
      CALL VVD ( ZOB, 1.407661405256671703D0, 1D-12,&
                 'ATCO13', 'ZOB', STATUS )
      CALL VVD ( HOB, -0.09265154431430045141D0, 1D-12,&
                 'ATCO13', 'HOB', STATUS )
      CALL VVD ( DOB, 0.1716626560074556029D0, 1D-12,&
                 'ATCO13', 'DOB', STATUS )
      CALL VVD ( ROB, 2.710260453503366591D0, 1D-12,&
                 'ATCO13', 'ROB', STATUS )
      CALL VVD ( EO, -0.003020548354802412839D0, 1D-14,&
                 'ATCO13', 'EO', STATUS )

      END

      SUBROUTINE T_ATIC13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T I C 1 3
!  - - - - - - - - - - - - -
!
!  Test ATIC13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATIC13, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RI, DI, DATE1, DATE2, RC, DC, EO


      RI = 2.710121572969038991D0
      DI = 0.1729371367218230438D0
      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0

      CALL ATIC13 ( RI, DI, DATE1, DATE2, RC, DC, EO )

      CALL VVD ( RC, 2.710126504531716819D0, 1D-12,&
                 'ATIC13', 'RC', STATUS )
      CALL VVD ( DC, 0.1740632537627034482D0, 1D-12,&
                 'ATIC13', 'DC', STATUS )
      CALL VVD ( EO, -0.002900618712657375647D0, 1D-14,&
                 'ATIC13', 'EO', STATUS )

      END

      SUBROUTINE T_ATICQ ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ A T I C Q
!  - - - - - - - - - - - -
!
!  Test ATICQ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, ATICQ, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO, RI, DI, RC, DC


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )
      RI = 2.710121572969038991D0
      DI = 0.1729371367218230438D0

      CALL ATICQ ( RI, DI, ASTROM, RC, DC )

      CALL VVD ( RC, 2.710126504531716819D0, 1D-12,&
                 'ATICQ', 'RC', STATUS )
      CALL VVD ( DC, 0.1740632537627034482D0, 1D-12,&
                 'ATICQ', 'DC', STATUS )

      END

      SUBROUTINE T_ATICQN ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T I C Q N
!  - - - - - - - - - - - - -
!
!  Test ATICQN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APCI13, ATICQN, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, ASTROM(30), EO, RI, DI, B(8,3),&
                       RC, DC


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      CALL APCI13 ( DATE1, DATE2, ASTROM, EO )
      RI = 2.709994899247599271D0
      DI = 0.1728740720983623469D0
      B(1,1) = 0.00028574D0
      B(2,1) = 3D-10
      B(3,1) = -7.81014427D0
      B(4,1) = -5.60956681D0
      B(5,1) = -1.98079819D0
      B(6,1) = 0.0030723249D0
      B(7,1) = -0.00406995477D0
      B(8,1) = -0.00181335842D0
      B(1,2) = 0.00095435D0
      B(2,2) = 3D-9
      B(3,2) = 0.738098796D0
      B(4,2) = 4.63658692D0
      B(5,2) = 1.9693136D0
      B(6,2) = -0.00755816922D0
      B(7,2) = 0.00126913722D0
      B(8,2) = 0.000727999001D0
      B(1,3) = 1D0
      B(2,3) = 6D-6
      B(3,3) = -0.000712174377D0
      B(4,3) = -0.00230478303D0
      B(5,3) = -0.00105865966D0
      B(6,3) = 6.29235213D-6
      B(7,3) = -3.30888387D-7
      B(8,3) = -2.96486623D-7

      CALL ATICQN ( RI, DI, ASTROM, 3, B, RC, DC )

      CALL VVD ( RC, 2.709999575033027333D0, 1D-12,&
                 'ATCIQN', 'RC', STATUS )
      CALL VVD ( DC, 0.1739999656316469990D0, 1D-12,&
                 'ATCIQN', 'DC', STATUS )

      END

      SUBROUTINE T_ATIO13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T I O 1 3
!  - - - - - - - - - - - - -
!
!  Test ATIO13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATIO13, VVD, VIV
!
!  This revision:  2013 October 2
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RI, DI, UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                       PHPA, TC, RH, WL, AOB, ZOB, HOB, DOB, ROB
      INTEGER J


      RI = 2.710121572969038991D0
      DI = 0.1729371367218230438D0
      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      CALL ATIO13 ( RI, DI, UTC1, UTC2, DUT1, ELONG, PHI, HM,&
                        XP, YP, PHPA, TC, RH, WL,&
                        AOB, ZOB, HOB, DOB, ROB, J )

      CALL VVD ( AOB, 0.09233952224794989993D0, 1D-12,&
                 'ATIO13', 'AOB', STATUS )
      CALL VVD ( ZOB, 1.407758704513722461D0, 1D-12,&
                 'ATIO13', 'ZOB', STATUS )
      CALL VVD ( HOB, -0.09247619879782006106D0, 1D-12,&
                 'ATIO13', 'HOB', STATUS )
      CALL VVD ( DOB, 0.1717653435758265198D0, 1D-12,&
                 'ATIO13', 'DOB', STATUS )
      CALL VVD ( ROB, 2.710085107986886201D0, 1D-12,&
                 'ATIO13', 'ROB', STATUS )
      CALL VIV ( J, 0, 'ATIO13', 'J', STATUS )

      END

      SUBROUTINE T_ATIOQ ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ A T I O Q
!  - - - - - - - - - - - -
!
!  Test ATIOQ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APIO13, ATIOQ, VVD, VIV
!
!  This revision:  2013 October 2
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                       PHPA, TC, RH, WL, ASTROM(30), RI, DI,&
                       AOB, ZOB, HOB, DOB, ROB
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0
      CALL APIO13 ( UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                        PHPA, TC, RH, WL, ASTROM, J )
      RI = 2.710121572969038991D0
      DI = 0.1729371367218230438D0

      CALL ATIOQ ( RI, DI, ASTROM, AOB, ZOB, HOB, DOB, ROB )

      CALL VVD ( AOB, 0.09233952224794989993D0, 1D-12,&
                 'ATIOQ', 'AOB', STATUS )
      CALL VVD ( ZOB, 1.407758704513722461D0, 1D-12,&
                 'ATIOQ', 'ZOB', STATUS )
      CALL VVD ( HOB, -0.09247619879782006106D0, 1D-12,&
                 'ATIOQ', 'HOB', STATUS )
      CALL VVD ( DOB, 0.1717653435758265198D0, 1D-12,&
                 'ATIOQ', 'DOB', STATUS )
      CALL VVD ( ROB, 2.710085107986886201D0, 1D-12,&
                 'ATIOQ', 'ROB', STATUS )

      END

      SUBROUTINE T_ATOC13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T O C 1 3
!  - - - - - - - - - - - - -
!
!  Test ATOC13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATOC13, VVD, VIV
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CHARACTER TYPE
      DOUBLE PRECISION UTC1, UTC2, DUT1,&
                       ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                       OB1, OB2, RC, DC
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      TYPE = 'R'
      OB1 = 2.710085107986886201D0
      OB2 = 0.1717653435758265198D0
      CALL ATOC13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RC, DC, J )
      CALL VVD ( RC, 2.709956744660731630D0, 1D-12,&
                 'ATOC13', 'R/RC', STATUS )
      CALL VVD ( DC, 0.1741696500896438967D0, 1D-12,&
                 'ATOC13', 'R/DC', STATUS )
      CALL VIV ( J, 0, 'ATOC13', 'R/J', STATUS )

      TYPE = 'H'
      OB1 = -0.09247619879782006106D0
      OB2 = 0.1717653435758265198D0
      CALL ATOC13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RC, DC, J )
      CALL VVD ( RC, 2.709956744660731630D0, 1D-12,&
                 'ATOC13', 'H/RC', STATUS )
      CALL VVD ( DC, 0.1741696500896438967D0, 1D-12,&
                 'ATOC13', 'H/DC', STATUS )
      CALL VIV ( J, 0, 'ATOC13', 'H/J', STATUS )

      TYPE = 'A'
      OB1 = 0.09233952224794989993D0
      OB2 = 1.407758704513722461D0
      CALL ATOC13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RC, DC, J )
      CALL VVD ( RC, 2.709956744660731630D0, 1D-12,&
                 'ATOC13', 'A/RC', STATUS )
      CALL VVD ( DC, 0.1741696500896438970D0, 1D-12,&
                 'ATOC13', 'A/DC', STATUS )
      CALL VIV ( J, 0, 'ATOC13', 'A/J', STATUS )

      END

      SUBROUTINE T_ATOI13 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ A T O I 1 3
!  - - - - - - - - - - - - -
!
!  Test ATOI13 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ATOI13, VVD, VIV
!
!  This revision:  2013 October 2
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CHARACTER TYPE
      DOUBLE PRECISION UTC1, UTC2, DUT1,&
                       ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                       OB1, OB2, RI, DI
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0

      TYPE = 'R'
      OB1 = 2.710085107986886201D0
      OB2 = 0.1717653435758265198D0
      CALL ATOI13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RI, DI, J )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOI13', 'R/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567725D0, 1D-12,&
                 'ATOI13', 'R/DI', STATUS )
      CALL VIV ( J, 0, 'ATOI13', 'R/J', STATUS )

      TYPE = 'H'
      OB1 = -0.09247619879782006106D0
      OB2 = 0.1717653435758265198D0
      CALL ATOI13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RI, DI, J )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOI13', 'H/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567725D0, 1D-12,&
                 'ATOI13', 'H/DI', STATUS )
      CALL VIV ( J, 0, 'ATOI13', 'H/J', STATUS )

      TYPE = 'A'
      OB1 = 0.09233952224794989993D0
      OB2 = 1.407758704513722461D0
      CALL ATOI13 ( TYPE, OB1, OB2, UTC1, UTC2, DUT1,&
                        ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,&
                        RI, DI, J )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOI13', 'A/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567728D0, 1D-12,&
                 'ATOI13', 'A/DI', STATUS )
      CALL VIV ( J, 0, 'ATOI13', 'A/J', STATUS )

      END

      SUBROUTINE T_ATOIQ ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ A T O I Q
!  - - - - - - - - - - - -
!
!  Test ATOIQ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  APIO13, ATOIQ, VVD
!
!  This revision:  2013 October 2
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CHARACTER TYPE
      DOUBLE PRECISION UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                       PHPA, TC, RH, WL, ASTROM(30),&
                       OB1, OB2, RI, DI
      INTEGER J


      UTC1 = 2456384.5D0
      UTC2 = 0.969254051D0
      DUT1 = 0.1550675D0
      ELONG = -0.527800806D0
      PHI = -1.2345856D0
      HM = 2738D0
      XP = 2.47230737D-7
      YP = 1.82640464D-6
      PHPA = 731D0
      TC = 12.8D0
      RH = 0.59D0
      WL = 0.55D0
      CALL APIO13 ( UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,&
                        PHPA, TC, RH, WL, ASTROM, J )

      TYPE = 'R'
      OB1 = 2.710085107986886201D0
      OB2 = 0.1717653435758265198D0
      CALL ATOIQ ( TYPE, OB1, OB2, ASTROM, RI, DI )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOIQ', 'R/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567725D0, 1D-12,&
                 'ATOIQ', 'R/DI', STATUS )

      TYPE = 'H'
      OB1 = -0.09247619879782006106D0
      OB2 = 0.1717653435758265198D0
      CALL ATOIQ ( TYPE, OB1, OB2, ASTROM, RI, DI )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOIQ', 'H/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567725D0, 1D-12,&
                 'ATOIQ', 'H/DI', STATUS )

      TYPE = 'A'
      OB1 = 0.09233952224794989993D0
      OB2 = 1.407758704513722461D0
      CALL ATOIQ ( TYPE, OB1, OB2, ASTROM, RI, DI )
      CALL VVD ( RI, 2.710121574449135955D0, 1D-12,&
                 'ATOIQ', 'A/RI', STATUS )
      CALL VVD ( DI, 0.1729371839114567728D0, 1D-12,&
                 'ATOIQ', 'A/DI', STATUS )

      END

      SUBROUTINE T_BI00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ B I 0 0
!  - - - - - - - - - - -
!
!  Test BI00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  BI00, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSIBI, DEPSBI, DRA


      CALL BI00 ( DPSIBI, DEPSBI, DRA )

      CALL VVD ( DPSIBI, -0.2025309152835086613D-6, 1D-12,&
                 'BI00', 'DPSIBI', STATUS )
      CALL VVD ( DEPSBI, -0.3306041454222147847D-7, 1D-12,&
                 'BI00', 'DEPSBI', STATUS )
      CALL VVD ( DRA, -0.7078279744199225506D-7, 1D-12,&
                 'BI00', 'DRA', STATUS )

      END

      SUBROUTINE T_BP00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ B P 0 0
!  - - - - - - - - - - -
!
!  Test BP00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  BP00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RB(3,3), RP(3,3), RBP(3,3)


      CALL BP00 ( 2400000.5D0, 50123.9999D0, RB, RP, RBP )

      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,&
                 'BP00', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,&
                 'BP00', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,&
                 'BP00', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,&
                 'BP00', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'BP00', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,&
                 'BP00', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,&
                 'BP00', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,&
                 'BP00', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'BP00', 'RB33', STATUS )

      CALL VVD ( RP(1,1), 0.9999995504864048241D0, 1D-12,&
                 'BP00', 'RP11', STATUS )
      CALL VVD ( RP(1,2), 0.8696113836207084411D-3, 1D-14,&
                 'BP00', 'RP12', STATUS )
      CALL VVD ( RP(1,3), 0.3778928813389333402D-3, 1D-14,&
                 'BP00', 'RP13', STATUS )
      CALL VVD ( RP(2,1), -0.8696113818227265968D-3, 1D-14,&
                 'BP00', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999996218879365258D0, 1D-12,&
                 'BP00', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.1690679263009242066D-6, 1D-14,&
                 'BP00', 'RP23', STATUS )
      CALL VVD ( RP(3,1), -0.3778928854764695214D-3, 1D-14,&
                 'BP00', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.1595521004195286491D-6, 1D-14,&
                 'BP00', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999999285984682756D0, 1D-12,&
                 'BP00', 'RP33', STATUS )

      CALL VVD ( RBP(1,1), 0.9999995505175087260D0, 1D-12,&
                 'BP00', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695405883617884705D-3, 1D-14,&
                 'BP00', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779734722239007105D-3, 1D-14,&
                 'BP00', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695405990410863719D-3, 1D-14,&
                 'BP00', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219494925900D0, 1D-12,&
                 'BP00', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.1360775820404982209D-6, 1D-14,&
                 'BP00', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734476558184991D-3, 1D-14,&
                 'BP00', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.1925857585832024058D-6, 1D-14,&
                 'BP00', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285680153377D0, 1D-12,&
                 'BP00', 'RBP33', STATUS )

      END

      SUBROUTINE T_BP06 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ B P 0 6
!  - - - - - - - - - - -
!
!  Test BP06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  BP06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RB(3,3), RP(3,3), RBP(3,3)


      CALL BP06 ( 2400000.5D0, 50123.9999D0, RB, RP, RBP )

      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,&
                 'BP06', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,&
                 'BP06', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,&
                 'BP06', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,&
                 'BP06', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'BP06', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,&
                 'BP06', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,&
                 'BP06', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,&
                 'BP06', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'BP06', 'RB33', STATUS )

      CALL VVD ( RP(1,1), 0.9999995504864960278D0, 1D-12,&
                 'BP06', 'RP11', STATUS )
      CALL VVD ( RP(1,2), 0.8696112578855404832D-3, 1D-14,&
                 'BP06', 'RP12', STATUS )
      CALL VVD ( RP(1,3), 0.3778929293341390127D-3, 1D-14,&
                 'BP06', 'RP13', STATUS )
      CALL VVD ( RP(2,1), -0.8696112560510186244D-3, 1D-14,&
                 'BP06', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999996218880458820D0, 1D-12,&
                 'BP06', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.1691646168941896285D-6, 1D-14,&
                 'BP06', 'RP23', STATUS )
      CALL VVD ( RP(3,1), -0.3778929335557603418D-3, 1D-14,&
                 'BP06', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.1594554040786495076D-6, 1D-14,&
                 'BP06', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999999285984501222D0, 1D-12,&
                 'BP06', 'RP33', STATUS )

      CALL VVD ( RBP(1,1), 0.9999995505176007047D0, 1D-12,&
                 'BP06', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695404617348208406D-3, 1D-14,&
                 'BP06', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779735201865589104D-3, 1D-14,&
                 'BP06', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695404723772031414D-3, 1D-14,&
                 'BP06', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219496027161D0, 1D-12,&
                 'BP06', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.1361752497080270143D-6, 1D-14,&
                 'BP06', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734957034089490D-3, 1D-14,&
                 'BP06', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.1924880847894457113D-6, 1D-14,&
                 'BP06', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285679971958D0, 1D-12,&
                 'BP06', 'RBP33', STATUS )

      END

      SUBROUTINE T_BPN2XY ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ B P N 2 X Y
!  - - - - - - - - - - - - -
!
!  Test BPN2XY routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  BPN2XY, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), X, Y


      RBPN(1,1) = 9.999962358680738D-1
      RBPN(1,2) = -2.516417057665452D-3
      RBPN(1,3) = -1.093569785342370D-3

      RBPN(2,1) = 2.516462370370876D-3
      RBPN(2,2) = 9.999968329010883D-1
      RBPN(2,3) = 4.006159587358310D-5

      RBPN(3,1) = 1.093465510215479D-3
      RBPN(3,2) = -4.281337229063151D-5
      RBPN(3,3) = 9.999994012499173D-1

      CALL BPN2XY ( RBPN, X, Y )

      CALL VVD ( X, 1.093465510215479D-3, 1D-12,&
                 'BPN2XY', 'X', STATUS )
      CALL VVD ( Y, -4.281337229063151D-5, 1D-12,&
                 'BPN2XY', 'Y', STATUS )

      END

      SUBROUTINE T_C2I00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 I 0 0 A
!  - - - - - - - - - - - - -
!
!  Test C2I00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2I00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL C2I00A ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037165557D0, 1D-12,&
                 'C2I00A', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526348992140183D-9, 1D-12,&
                 'C2I00A', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308477073443415D-3, 1D-12,&
                 'C2I00A', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384266227870752452D-7, 1D-12,&
                 'C2I00A', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917405258D0, 1D-12,&
                 'C2I00A', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020594955028209745D-4, 1D-12,&
                 'C2I00A', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308472168152904D-3, 1D-12,&
                 'C2I00A', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020595661591500259D-4, 1D-12,&
                 'C2I00A', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954572304D0, 1D-12,&
                 'C2I00A', '33', STATUS )

      END

      SUBROUTINE T_C2I00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 I 0 0 B
!  - - - - - - - - - - - - -
!
!  Test C2I00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2I00B, VVD
!
!  This revision:  2008 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL C2I00B ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323040954356D0, 1D-12,&
                 'C2I00B', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526349131823372D-9, 1D-12,&
                 'C2I00B', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791301934855394005D-3, 1D-12,&
                 'C2I00B', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384239285499175543D-7, 1D-12,&
                 'C2I00B', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917574043D0, 1D-12,&
                 'C2I00B', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020552974819030066D-4, 1D-12,&
                 'C2I00B', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791301929950208873D-3, 1D-12,&
                 'C2I00B', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020553681373720832D-4, 1D-12,&
                 'C2I00B', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314958529887D0, 1D-12,&
                 'C2I00B', '33', STATUS )

      END

      SUBROUTINE T_C2I06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 I 0 6 A
!  - - - - - - - - - - - - -
!
!  Test C2I06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2I06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL C2I06A ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037159379D0, 1D-12,&
                 'C2I06A', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581121329587613787D-9, 1D-12,&
                 'C2I06A', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308487740529749D-3, 1D-12,&
                 'C2I06A', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384253169452306581D-7, 1D-12,&
                 'C2I06A', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917467827D0, 1D-12,&
                 'C2I06A', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579392895682558D-4, 1D-12,&
                 'C2I06A', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308482835292617D-3, 1D-12,&
                 'C2I06A', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020580099454020310D-4, 1D-12,&
                 'C2I06A', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954628695D0, 1D-12,&
                 'C2I06A', '33', STATUS )

      END

      SUBROUTINE T_C2IBPN ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 I B P N
!  - - - - - - - - - - - - -
!
!  Test C2IBPN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2IBPN, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), RC2I(3,3)


      RBPN(1,1) = 9.999962358680738D-1
      RBPN(1,2) = -2.516417057665452D-3
      RBPN(1,3) = -1.093569785342370D-3

      RBPN(2,1) = 2.516462370370876D-3
      RBPN(2,2) = 9.999968329010883D-1
      RBPN(2,3) = 4.006159587358310D-5

      RBPN(3,1) = 1.093465510215479D-3
      RBPN(3,2) = -4.281337229063151D-5
      RBPN(3,3) = 9.999994012499173D-1

      CALL C2IBPN ( 2400000.5D0, 50123.9999D0, RBPN, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999994021664089977D0, 1D-12,&
                 'C2IBPN', '11', STATUS )
      CALL VVD ( RC2I(1,2), -0.3869195948017503664D-8, 1D-12,&
                 'C2IBPN', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.1093465511383285076D-2, 1D-12,&
                 'C2IBPN', '13', STATUS )
      CALL VVD ( RC2I(2,1), 0.5068413965715446111D-7, 1D-12,&
                 'C2IBPN', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999990835075686D0, 1D-12,&
                 'C2IBPN', '22', STATUS )
      CALL VVD ( RC2I(2,3), 0.4281334246452708915D-4, 1D-12,&
                 'C2IBPN', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.1093465510215479000D-2, 1D-12,&
                 'C2IBPN', '31', STATUS )
      CALL VVD ( RC2I(3,2), -0.4281337229063151000D-4, 1D-12,&
                 'C2IBPN', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999994012499173103D0, 1D-12,&
                 'C2IBPN', '33', STATUS )

      END

      SUBROUTINE T_C2IXY ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ C 2 I X Y
!  - - - - - - - - - - - -
!
!  Test C2IXY routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2IXY, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, RC2I(3,3)


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL C2IXY ( 2400000.5D0, 53736D0, X, Y, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037157138D0, 1D-12,&
                 'C2IXY', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526349032241205D-9, 1D-12,&
                 'C2IXY', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308491611263745D-3, 1D-12,&
                 'C2IXY', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384257057469842953D-7, 1D-12,&
                 'C2IXY', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917468964D0, 1D-12,&
                 'C2IXY', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579110172324363D-4, 1D-12,&
                 'C2IXY', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308486706011000D-3, 1D-12,&
                 'C2IXY', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020579816732961219D-4, 1D-12,&
                 'C2IXY', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954627590D0, 1D-12,&
                 'C2IXY', '33', STATUS )

      END

      SUBROUTINE T_C2IXYS ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 I X Y S
!  - - - - - - - - - - - - -
!
!  Test C2IXYS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2IXYS, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, S, RC2I(3,3)


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4
      S = -0.1220040848472271978D-7

      CALL C2IXYS ( X, Y, S, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037157138D0, 1D-12,&
                 'C2IXYS', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581984869168499149D-9, 1D-12,&
                 'C2IXYS', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308491611282180D-3, 1D-12,&
                 'C2IXYS', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384261642670440317D-7, 1D-12,&
                 'C2IXYS', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917468964D0, 1D-12,&
                 'C2IXYS', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579110169668931D-4, 1D-12,&
                 'C2IXYS', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308486706011000D-3, 1D-12,&
                 'C2IXYS', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020579816732961219D-4, 1D-12,&
                 'C2IXYS', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954627590D0, 1D-12,&
                 'C2IXYS', '33', STATUS )

      END

      SUBROUTINE T_C2S ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ C 2 S
!  - - - - - - - - - -
!
!  Test C2S routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2S, VVD
!
!  This revision:   2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), THETA, PHI


      P(1) = 100D0
      P(2) = -50D0
      P(3) = 25D0

      CALL C2S ( P, THETA, PHI )

      CALL VVD ( THETA, -0.4636476090008061162D0, 1D-14,&
                 'C2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.2199879773954594463D0, 1D-14,&
                 'C2S', 'PHI', STATUS )

      END

      SUBROUTINE T_C2T00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 T 0 0 A
!  - - - - - - - - - - - - -
!
!  Test C2T00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2T00A, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL C2T00A ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128307182668D0, 1D-12,&
                 'C2T00A', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938457836D0, 1D-12,&
                 'C2T00A', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535638688341725D-4, 1D-12,&
                 'C2T00A', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134135984552D0, 1D-12,&
                 'C2T00A', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649520727D0, 1D-12,&
                 'C2T00A', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116141056317D-3, 1D-12,&
                 'C2T00A', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081406921D-3, 1D-12,&
                 'C2T00A', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391770163647D-4, 1D-12,&
                 'C2T00A', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501692289D0, 1D-12,&
                 'C2T00A', '33', STATUS )

      END

      SUBROUTINE T_C2T00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 T 0 0 B
!  - - - - - - - - - - - - -
!
!  Test C2T00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2T00B, VVD
!
!  This revision:  2008 November 29
!-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL C2T00B ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128439678965D0, 1D-12,&
                 'C2T00B', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806913872359D0, 1D-12,&
                 'C2T00B', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555565082458415611D-4, 1D-12,&
                 'C2T00B', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134115435923D0, 1D-12,&
                 'C2T00B', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203784001946D0, 1D-12,&
                 'C2T00B', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749793922030017230D-3, 1D-12,&
                 'C2T00B', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773467471863534901D-3, 1D-12,&
                 'C2T00B', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961790411549945020D-4, 1D-12,&
                 'C2T00B', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325505635738D0, 1D-12,&
                 'C2T00B', '33', STATUS )

      END

      SUBROUTINE T_C2T06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 T 0 6 A
!  - - - - - - - - - - - - -
!
!  Test C2T06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2T06A, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL C2T06A ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128305897282D0, 1D-12,&
                 'C2T06A', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938592296D0, 1D-12,&
                 'C2T06A', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555550962998436505D-4, 1D-12,&
                 'C2T06A', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134136214897D0, 1D-12,&
                 'C2T06A', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649130832D0, 1D-12,&
                 'C2T06A', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749800844905594110D-3, 1D-12,&
                 'C2T06A', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474024748545878D-3, 1D-12,&
                 'C2T06A', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961816829632690581D-4, 1D-12,&
                 'C2T06A', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501747785D0, 1D-12,&
                 'C2T06A', '33', STATUS )

      END

      SUBROUTINE T_C2TCIO ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 T C I O
!  - - - - - - - - - - - - -
!
!  Test C2TCIO routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2TCIO, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3), ERA, RPOM(3,3), RC2T(3,3)


      RC2I(1,1) = 0.9999998323037164738D0
      RC2I(1,2) = 0.5581526271714303683D-9
      RC2I(1,3) = -0.5791308477073443903D-3

      RC2I(2,1) = -0.2384266227524722273D-7
      RC2I(2,2) = 0.9999999991917404296D0
      RC2I(2,3) = -0.4020594955030704125D-4

      RC2I(3,1) = 0.5791308472168153320D-3
      RC2I(3,2) = 0.4020595661593994396D-4
      RC2I(3,3) = 0.9999998314954572365D0

      ERA = 1.75283325530307D0

      RPOM(1,1) = 0.9999999999999674705D0
      RPOM(1,2) = -0.1367174580728847031D-10
      RPOM(1,3) = 0.2550602379999972723D-6

      RPOM(2,1) = 0.1414624947957029721D-10
      RPOM(2,2) = 0.9999999999982694954D0
      RPOM(2,3) = -0.1860359246998866338D-5

      RPOM(3,1) = -0.2550602379741215275D-6
      RPOM(3,2) = 0.1860359247002413923D-5
      RPOM(3,3) = 0.9999999999982369658D0

      CALL C2TCIO ( RC2I, ERA, RPOM, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128307110439D0, 1D-12,&
                 'C2TCIO', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938470149D0, 1D-12,&
                 'C2TCIO', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535638685466874D-4, 1D-12,&
                 'C2TCIO', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134135996657D0, 1D-12,&
                 'C2TCIO', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649448367D0, 1D-12,&
                 'C2TCIO', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116141106528D-3, 1D-12,&
                 'C2TCIO', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081407076D-3, 1D-12,&
                 'C2TCIO', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391772658944D-4, 1D-12,&
                 'C2TCIO', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501691969D0, 1D-12,&
                 'C2TCIO', '33', STATUS )

      END

      SUBROUTINE T_C2TEQX ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C 2 T E Q X
!  - - - - - - - - - - - - -
!
!  Test C2TEQX routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2TEQX, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), GST, RPOM(3,3), RC2T(3,3)


      RBPN(1,1) = 0.9999989440476103608D0
      RBPN(1,2) = -0.1332881761240011518D-2
      RBPN(1,3) = -0.5790767434730085097D-3

      RBPN(2,1) = 0.1332858254308954453D-2
      RBPN(2,2) = 0.9999991109044505944D0
      RBPN(2,3) = -0.4097782710401555759D-4

      RBPN(3,1) = 0.5791308472168153320D-3
      RBPN(3,2) = 0.4020595661593994396D-4
      RBPN(3,3) = 0.9999998314954572365D0

      GST = 1.754166138040730516D0

      RPOM(1,1) = 0.9999999999999674705D0
      RPOM(1,2) = -0.1367174580728847031D-10
      RPOM(1,3) = 0.2550602379999972723D-6

      RPOM(2,1) = 0.1414624947957029721D-10
      RPOM(2,2) = 0.9999999999982694954D0
      RPOM(2,3) = -0.1860359246998866338D-5

      RPOM(3,1) = -0.2550602379741215275D-6
      RPOM(3,2) = 0.1860359247002413923D-5
      RPOM(3,3) = 0.9999999999982369658D0

      CALL C2TEQX ( RBPN, GST, RPOM, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128528685730D0, 1D-12,&
                 'C2TEQX', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806897685071D0, 1D-12,&
                 'C2TEQX', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535639982634449D-4, 1D-12,&
                 'C2TEQX', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134095211257D0, 1D-12,&
                 'C2TEQX', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203871023800D0, 1D-12,&
                 'C2TEQX', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116126438962D-3, 1D-12,&
                 'C2TEQX', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081539467D-3, 1D-12,&
                 'C2TEQX', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391768640871D-4, 1D-12,&
                 'C2TEQX', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501691969D0, 1D-12,&
                 'C2TEQX', '33', STATUS )

      END

      SUBROUTINE T_C2TPE ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ C 2 T P E
!  - - - - - - - - - - - -
!
!  Test C2TPE routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2TPE, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      DEPS = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL C2TPE ( TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1813677995763029394D0, 1D-12,&
                 'C2TPE', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9023482206891683275D0, 1D-12,&
                 'C2TPE', '12', STATUS )
      CALL VVD ( RC2T(1,3), -0.3909902938641085751D0, 1D-12,&
                 'C2TPE', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834147641476804807D0, 1D-12,&
                 'C2TPE', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1659883635434995121D0, 1D-12,&
                 'C2TPE', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.7309763898042819705D-1, 1D-12,&
                 'C2TPE', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.1059685430673215247D-2, 1D-12,&
                 'C2TPE', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3977631855605078674D0, 1D-12,&
                 'C2TPE', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9174875068792735362D0, 1D-12,&
                 'C2TPE', '33', STATUS )

      END

      SUBROUTINE T_C2TXY ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ C 2 T X Y
!  - - - - - - - - - - - -
!
!  Test C2TXY routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  C2TXY, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, X, Y, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL C2TXY ( TTA, TTB, UTA, UTB, X, Y, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128306279253D0, 1D-12,&
                 'C2TXY', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938520084D0, 1D-12,&
                 'C2TXY', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555551248057665829D-4, 1D-12,&
                 'C2TXY', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134136142314D0, 1D-12,&
                 'C2TXY', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649529312D0, 1D-12,&
                 'C2TXY', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749800843594139912D-3, 1D-12,&
                 'C2TXY', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474028619264494D-3, 1D-12,&
                 'C2TXY', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961816546911624260D-4, 1D-12,&
                 'C2TXY', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501746670D0, 1D-12,&
                 'C2TXY', '33', STATUS )

      END

      SUBROUTINE T_CAL2JD ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ C A L 2 J D
!  - - - - - - - - - - - - -
!
!  Test CAL2JD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  CAL2JD, VVD, VIV
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION DJM0, DJM


      CALL CAL2JD ( 2003, 06, 01, DJM0, DJM, J )

      CALL VVD ( DJM0 + DJM, 2452791.5D0, 0D0,&
                 'CAL2JD', 'JD + MJD', STATUS )
      CALL VIV ( J, 0, 'CAL2JD', 'J', STATUS )

      END

      SUBROUTINE T_CP ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ C P
!  - - - - - - - - -
!
!  Test CP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  CP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), C(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL CP ( P, C )

      CALL VVD ( C(1), 0.3D0, 0D0, 'CP', '1', STATUS )
      CALL VVD ( C(2), 1.2D0, 0D0, 'CP', '2', STATUS )
      CALL VVD ( C(3), -2.5D0, 0D0, 'CP', '3', STATUS )

      END

      SUBROUTINE T_CPV ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ C P V
!  - - - - - - - - - -
!
!  Test CPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  CPV, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), C(3,2)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL CPV ( PV, C )

      CALL VVD ( C(1,1), 0.3D0, 0D0, 'CPV', 'P1', STATUS )
      CALL VVD ( C(2,1), 1.2D0, 0D0, 'CPV', 'P2', STATUS )
      CALL VVD ( C(3,1), -2.5D0, 0D0, 'CPV', 'P3', STATUS )
      CALL VVD ( C(1,2), -0.5D0, 0D0, 'CPV', 'V1', STATUS )
      CALL VVD ( C(2,2), 3.1D0, 0D0, 'CPV', 'V2', STATUS )
      CALL VVD ( C(3,2), 0.9D0, 0D0, 'CPV', 'V3', STATUS )

      END

      SUBROUTINE T_CR ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ C R
!  - - - - - - - - -
!
!  Test CR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  CR, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), C(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL CR ( R, C )

      CALL VVD ( C(1,1), 2D0, 0D0, 'CR', '11', STATUS )
      CALL VVD ( C(1,2), 3D0, 0D0, 'CR', '12', STATUS )
      CALL VVD ( C(1,3), 2D0, 0D0, 'CR', '13', STATUS )
      CALL VVD ( C(2,1), 3D0, 0D0, 'CR', '21', STATUS )
      CALL VVD ( C(2,2), 2D0, 0D0, 'CR', '22', STATUS )
      CALL VVD ( C(2,3), 3D0, 0D0, 'CR', '23', STATUS )
      CALL VVD ( C(3,1), 3D0, 0D0, 'CR', '31', STATUS )
      CALL VVD ( C(3,2), 4D0, 0D0, 'CR', '32', STATUS )
      CALL VVD ( C(3,3), 5D0, 0D0, 'CR', '33', STATUS )

      END

      SUBROUTINE T_D2DTF ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ D 2 D T F
!  - - - - - - - - - - - -
!
!  Test D2DTF routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  D2DTF, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IY, IM, ID, IHMSF(4), J


      CALL D2DTF ( 'UTC', 5, 2400000.5D0, 49533.99999D0,&
                       IY, IM, ID, IHMSF, J )

      CALL VIV ( IY, 1994, 'D2DTF', 'Y', STATUS )
      CALL VIV ( IM, 6, 'D2DTF', 'Mo', STATUS )
      CALL VIV ( ID, 30, 'D2DTF', 'D', STATUS )
      CALL VIV ( IHMSF(1), 23, 'D2DTF', 'H', STATUS )
      CALL VIV ( IHMSF(2), 59, 'D2DTF', 'M', STATUS )
      CALL VIV ( IHMSF(3), 60, 'D2DTF', 'S', STATUS )
      CALL VIV ( IHMSF(4), 13599, 'D2DTF', 'F', STATUS )
      CALL VIV ( J, 0, 'D2DTF', 'J', STATUS )

      END

      SUBROUTINE T_D2TF ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ D 2 T F
!  - - - - - - - - - - -
!
!  Test D2TF routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  D2TF, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IHMSF(4)
      CHARACTER S


      CALL D2TF ( 4, -0.987654321D0, S, IHMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '-' ), 'D2TF', 'S', STATUS )
      CALL VIV ( IHMSF(1), 23, 'D2TF', '1', STATUS )
      CALL VIV ( IHMSF(2), 42, 'D2TF', '2', STATUS )
      CALL VIV ( IHMSF(3), 13, 'D2TF', '3', STATUS )
      CALL VIV ( IHMSF(4), 3333, 'D2TF', '4', STATUS )

      END

      SUBROUTINE T_DAT ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ D A T
!  - - - - - - - - - -
!
!  Test DAT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  DAT, VVD, VIV
!
!  This revision:  2016 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION DELTAT


      CALL DAT ( 2003, 06, 01, 0D0, DELTAT, J )
      CALL VVD ( DELTAT, 32D0, 0D0, 'DAT', 'DELTAT', STATUS )
      CALL VIV ( J, 0, 'DAT', 'J', STATUS )
      CALL DAT ( 2008, 01, 17, 0D0, DELTAT, J )
      CALL VVD ( DELTAT, 33D0, 0D0, 'DAT', 'DELTAT', STATUS )
      CALL VIV ( J, 0, 'DAT', 'J', STATUS )
      CALL DAT ( 2017, 09, 01, 0D0, DELTAT, J )
      CALL VVD ( DELTAT, 37D0, 0D0, 'DAT', 'DELTAT', STATUS )
      CALL VIV ( J, 0, 'DAT', 'J', STATUS )

      END

      SUBROUTINE T_DTDB ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ D T D B
!  - - - - - - - - - - -
!
!  Test DTDB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  DTDB, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( DTDB ( 2448939.5D0, 0.123D0,&
                            0.76543D0, 5.0123D0, 5525.242D0, 3190D0 ),&
                 -0.1280368005936998991D-2, 1D-15,&
                 'DTDB', ' ', STATUS )

      END

      SUBROUTINE T_DTF2D ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ D T F 2 D
!  - - - - - - - - - - - -
!
!  Test DTF2D routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  DTF2D, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL DTF2D ( 'UTC', 1994, 6, 30, 23, 59, 60.13599D0,&
                       U1, U2, J )

      CALL VVD ( U1+U2,&
                 2449534.49999D0, 1D-6, 'DTF2D', 'U', STATUS )
      CALL VIV ( J, 0, 'DTF2D', 'J', STATUS )

      END

      SUBROUTINE T_ECEQ06 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E C E Q 0 6
!  - - - - - - - - - - - - -
!
!  Test ECEQ06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ECEQ06, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, DL, DB, DR, DD


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0
      DL = 5.1D0
      DB = -0.9D0

      CALL ECEQ06 ( DATE1, DATE2, DL, DB, DR, DD )

      CALL VVD ( DR, 5.533459733613627767D0, 1D-14,&
                 'ECEQ06', 'DR', STATUS )
      CALL VVD ( DD, -1.246542932554480576D0, 1D-14,&
                 'ECEQ06', 'DD', STATUS )

      END

      SUBROUTINE T_ECM06 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E C M 0 6
!  - - - - - - - - - - - -
!
!  Test ECM06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ECM06, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, RM(3,3)


      DATE1 = 2456165.5D0
      DATE2 = 0.401182685D0

      CALL ECM06 ( DATE1, DATE2, RM )

      CALL VVD ( RM(1,1), 0.9999952427708701137D0, 1D-14,&
                 'ECM06', 'RM11', STATUS )
      CALL VVD ( RM(1,2), -0.2829062057663042347D-2, 1D-14,&
                 'ECM06', 'RM12', STATUS )
      CALL VVD ( RM(1,3), -0.1229163741100017629D-2, 1D-14,&
                 'ECM06', 'RM13', STATUS )
      CALL VVD ( RM(2,1), 0.3084546876908653562D-2, 1D-14,&
                 'ECM06', 'RM21', STATUS )
      CALL VVD ( RM(2,2), 0.9174891871550392514D0, 1D-14,&
                 'ECM06', 'RM22', STATUS )
      CALL VVD ( RM(2,3), 0.3977487611849338124D0, 1D-14,&
                 'ECM06', 'RM23', STATUS )
      CALL VVD ( RM(3,1), 0.2488512951527405928D-5, 1D-14,&
                 'ECM06', 'RM31', STATUS )
      CALL VVD ( RM(3,2), -0.3977506604161195467D0, 1D-14,&
                 'ECM06', 'RM32', STATUS )
      CALL VVD ( RM(3,3), 0.9174935488232863071D0, 1D-14,&
                 'ECM06', 'RM33', STATUS )

      END

      SUBROUTINE T_EE00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ E E 0 0
!  - - - - - - - - - - -
!
!  Test EE00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EE00, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPSA, DPSI

      EPSA = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5

      CALL VVD ( EE00 ( 2400000.5D0, 53736D0, EPSA, DPSI ),&
                 -0.8834193235367965479D-5, 1D-18,&
                 'EE00', ' ', STATUS )

      END

      SUBROUTINE T_EE00A ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E E 0 0 A
!  - - - - - - - - - - - -
!
!  Test EE00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EE00A, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EE00A ( 2400000.5D0, 53736D0 ),&
                 -0.8834192459222588227D-5, 1D-18,&
                 'EE00A', ' ', STATUS )

      END

      SUBROUTINE T_EE00B ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E E 0 0 B
!  - - - - - - - - - - - -
!
!  Test EE00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EE00B, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EE00B ( 2400000.5D0, 53736D0 ),&
                 -0.8835700060003032831D-5, 1D-18,&
                 'EE00B', ' ', STATUS )

      END

      SUBROUTINE T_EE06A ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E E 0 6 A
!  - - - - - - - - - - - -
!
!  Test EE06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EE06A, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EE06A ( 2400000.5D0, 53736D0 ),&
                 -0.8834195072043790156D-5, 1D-15,&
                 'EE06A', ' ', STATUS )

      END

      SUBROUTINE T_EECT00 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E E C T 0 0
!  - - - - - - - - - - - - -
!
!  Test EECT00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EECT00, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EECT00 ( 2400000.5D0, 53736D0 ),&
                 0.2046085004885125264D-8, 1D-20,&
                 'EECT00', ' ', STATUS )

      END

      SUBROUTINE T_EFORM ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E F O R M
!  - - - - - - - - - - - -
!
!  Test EFORM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EFORM, VIV, VVD
!
!  This revision:  2016 March 12
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION A, F


      CALL EFORM ( 0, A, F, J )

      CALL VIV ( J, -1, 'EFORM', 'J0', STATUS )

      CALL EFORM ( 1, A, F, J )

      CALL VIV ( J, 0, 'EFORM', 'J1', STATUS )
      CALL VVD ( A, 6378137D0, 1D-10, 'EFORM', 'A1', STATUS )
      CALL VVD ( F, 0.3352810664747480720D-2, 1D-18,&
                 'EFORM', 'F1', STATUS )

      CALL EFORM( 2, A, F, J )

      CALL VIV ( J, 0, 'EFORM', 'J2', STATUS )
      CALL VVD ( A, 6378137D0, 1D-10, 'EFORM', 'A2', STATUS )
      CALL VVD ( F, 0.3352810681182318935D-2, 1D-18,&
                 'EFORM', 'F2', STATUS )

      CALL EFORM( 3, A, F, J )

      CALL VIV ( J, 0, 'EFORM', 'J3', STATUS )
      CALL VVD ( A, 6378135D0, 1D-10, 'EFORM', 'A3', STATUS )
      CALL VVD ( F, 0.3352779454167504862D-2, 1D-18,&
                 'EFORM', 'F3', STATUS )

      CALL EFORM( 4, A, F, J )
      CALL VIV ( J, -1, 'EFORM', 'J4', STATUS )

      END

      SUBROUTINE T_EO06A ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E O 0 6 A
!  - - - - - - - - - - - -
!
!  Test EO06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EO06A, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EO06A ( 2400000.5D0, 53736D0 ),&
                 -0.1332882371941833644D-2, 1D-15,&
                 'EO06A', ' ', STATUS )

      END

      SUBROUTINE T_EORS ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ E O R S
!  - - - - - - - - - - -
!
!  Test EORS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EORS, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RNPB(3,3), S


      RNPB(1,1) = 0.9999989440476103608D0
      RNPB(1,2) = -0.1332881761240011518D-2
      RNPB(1,3) = -0.5790767434730085097D-3

      RNPB(2,1) = 0.1332858254308954453D-2
      RNPB(2,2) = 0.9999991109044505944D0
      RNPB(2,3) = -0.4097782710401555759D-4

      RNPB(3,1) = 0.5791308472168153320D-3
      RNPB(3,2) = 0.4020595661593994396D-4
      RNPB(3,3) = 0.9999998314954572365D0

      S = -0.1220040848472271978D-7

      CALL VVD ( EORS ( RNPB, S ), -0.1332882715130744606D-2, 1D-14,&
                 'EORS', ' ', STATUS )

      END

      SUBROUTINE T_EPB ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ E P B
!  - - - - - - - - - -
!
!  Test EPB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EPB, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EPB ( 2415019.8135D0, 30103.18648D0 ),&
                 1982.418424159278580D0, 1D-12, 'EPB', ' ', STATUS )

      END

      SUBROUTINE T_EPB2JD ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E P B 2 J D
!  - - - - - - - - - - - - -
!
!  Test EPB2JD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EPB2JD, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPB, DJM0, DJM


      EPB = 1957.3D0

      CALL EPB2JD ( EPB, DJM0, DJM )
      CALL VVD ( DJM0, 2400000.5D0, 1D-9,&
                 'EPB2JD', 'DJM0', STATUS )
      CALL VVD ( DJM, 35948.1915101513D0, 1D-9,&
                 'EPB2JD', 'MJD', STATUS )

      END

      SUBROUTINE T_EPJ ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ E P J
!  - - - - - - - - - -
!
!  Test EPJ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EPJ, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EPJ ( 2451545D0, -7392.5D0 ),&
                 1979.760438056125941D0, 1D-12, 'EPJ', ' ', STATUS )

      END

      SUBROUTINE T_EPJ2JD ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E P J 2 J D
!  - - - - - - - - - - -  -
!
!  Test EPJ2JD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EPJ2JD, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, DJM0, DJM


      EPJ = 1996.8D0

      CALL EPJ2JD ( EPJ, DJM0, DJM )

      CALL VVD ( DJM0, 2400000.5D0, 1D-9,&
                 'EPJ2JD', 'DJM0', STATUS )
      CALL VVD ( DJM, 50375.7D0, 1D-9,&
                 'EPJ2JD', 'MJD', STATUS )

      END

      SUBROUTINE T_EPV00 ( STATUS )
!+
!  - - - - - - - -
!   T _ E P V 0 0
!  - - - - - - - -
!
!  Test EPV00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called: EPV00, VVD, VIV
!
!  This revision:  2009 July 11
!-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PVH(3,2), PVB(3,2)
      INTEGER JSTAT


      CALL EPV00 ( 2400000.5D0, 53411.52501161D0, PVH, PVB,&
                       JSTAT )

      CALL VVD ( PVH(1,1), -0.7757238809297706813D0, 1D-14,&
                 'EPV00', 'PVH(X)', STATUS )
      CALL VVD ( PVH(2,1), +0.5598052241363340596D0, 1D-14,&
                 'EPV00', 'PVH(Y)', STATUS )
      CALL VVD ( PVH(3,1), +0.2426998466481686993D0, 1D-14,&
                 'EPV00', 'PVH(Z)', STATUS )
      CALL VVD ( PVH(1,2), -0.1091891824147313846D-1, 1D-15,&
                'EPV00', 'PVH(X)', STATUS )
      CALL VVD ( PVH(2,2), -0.1247187268440845008D-1, 1D-15,&
                'EPV00', 'PVH(Y)', STATUS )
      CALL VVD ( PVH(3,2), -0.5407569418065039061D-2, 1D-15,&
                'EPV00', 'PVH(Z)', STATUS )
      CALL VVD ( PVB(1,1), -0.7714104440491111971D0, 1D-14,&
                 'EPV00', 'PVB(X)', STATUS )
      CALL VVD ( PVB(2,1), +0.5598412061824171323D0, 1D-14,&
                 'EPV00', 'PVB(Y)', STATUS )
      CALL VVD ( PVB(3,1), +0.2425996277722452400D0, 1D-14,&
                 'EPV00', 'PVB(Z)', STATUS )
      CALL VVD ( PVB(1,2), -0.1091874268116823295D-1, 1D-15,&
                 'EPV00', 'PVB(X)', STATUS )
      CALL VVD ( PVB(2,2), -0.1246525461732861538D-1, 1D-15,&
                 'EPV00', 'PVB(Y)', STATUS )
      CALL VVD ( PVB(3,2), -0.5404773180966231279D-2, 1D-15,&
                 'EPV00', 'PVB(Z)', STATUS )
      CALL VIV ( JSTAT, 0, 'EPV00', 'JSTAT', STATUS )

      END

      SUBROUTINE T_EQEC06 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E Q E C 0 6
!  - - - - - - - - - - - - -
!
!  Test EQEC06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EQEC06, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DATE1, DATE2, DR, DD, DL, DB


      DATE1 = 1234.5D0
      DATE2 = 2440000.5D0
      DR = 1.234D0
      DD = 0.987D0

      CALL EQEC06 ( DATE1, DATE2, DR, DD, DL, DB )

      CALL VVD ( DL, 1.342509918994654619D0, 1D-14,&
                 'EQEC06', 'DL', STATUS )
      CALL VVD ( DB, 0.5926215259704608132D0, 1D-14,&
                 'EQEC06', 'DB', STATUS )

      END

      SUBROUTINE T_EQEQ94 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ E Q E Q 9 4
!  - - - - - - - - - - - - -
!
!  Test EQEQ94 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  EQEQ94, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( EQEQ94 ( 2400000.5D0, 41234D0 ),&
                 0.5357758254609256894D-4, 1D-17,&
                 'EQEQ94', ' ', STATUS )

      END

      SUBROUTINE T_ERA00 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ E R A 0 0
!  - - - - - - - - - - - -
!
!  Test ERA00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ERA00, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( ERA00 ( 2400000.5D0, 54388D0 ),&
                 0.4022837240028158102D0, 1D-12,&
                 'ERA00', ' ', STATUS )

      END

      SUBROUTINE T_FAD03 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F A D 0 3
!  - - - - - - - - - - - -
!
!  Test FAD03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAD03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAD03 ( 0.80D0 ),&
                 1.946709205396925672D0, 1D-12,&
                 'FAD03', ' ', STATUS )

      END

      SUBROUTINE T_FAE03 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F A E 0 3
!  - - - - - - - - - - - -
!
!  Test FAE03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAE03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAE03 ( 0.80D0 ),&
                 1.744713738913081846D0, 1D-12,&
                 'FAE03', ' ', STATUS )

      END

      SUBROUTINE T_FAF03 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F A F 0 3
!  - - - - - - - - - - - -
!
!  Test FAF03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAF03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAF03 ( 0.80D0 ),&
                 0.2597711366745499518D0, 1D-12,&
                 'FAF03', ' ', STATUS )

      END

      SUBROUTINE T_FAJU03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A J U 0 3
!  - - - - - - - - - - - - -
!
!  Test FAJU03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAJU03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAJU03 ( 0.80D0 ),&
                 5.275711665202481138D0, 1D-12,&
                 'FAJU03', ' ', STATUS )

      END

      SUBROUTINE T_FAL03 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F A L 0 3
!  - - - - - - - - - - - -
!
!  Test FAL03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAL03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAL03 ( 0.80D0 ),&
                 5.132369751108684150D0, 1D-12,&
                 'FAL03', ' ', STATUS )

      END

      SUBROUTINE T_FALP03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A L P 0 3
!  - - - - - - - - - - - - -
!
!  Test FALP03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FALP03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FALP03 ( 0.80D0 ),&
                 6.226797973505507345D0, 1D-12,&
                 'FALP03', ' ', STATUS )

      END

      SUBROUTINE T_FAMA03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A M A 0 3
!  - - - - - - - - - - - - -
!
!  Test FAMA03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAMA03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAMA03 ( 0.80D0 ),&
                 3.275506840277781492D0, 1D-12,&
                 'FAMA03', ' ', STATUS )

      END

      SUBROUTINE T_FAME03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A M E 0 3
!  - - - - - - - - - - - - -
!
!  Test FAME03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAME03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAME03 ( 0.80D0 ),&
                 5.417338184297289661D0, 1D-12,&
                 'FAME03', ' ', STATUS )

      END

      SUBROUTINE T_FANE03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A N E 0 3
!  - - - - - - - - - - - - -
!
!  Test FANE03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FANE03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FANE03 ( 0.80D0 ),&
                 2.079343830860413523D0, 1D-12,&
                 'FANE03', ' ', STATUS )

      END

      SUBROUTINE T_FAOM03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A O M 0 3
!  - - - - - - - - - - - - -
!
!  Test FAOM03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAOM03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAOM03 ( 0.80D0 ),&
                 -5.973618440951302183D0, 1D-12,&
                 'FAOM03', ' ', STATUS )

      END

      SUBROUTINE T_FAPA03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A P A 0 3
!  - - - - - - - - - - - - -
!
!  Test FAPA03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAPA03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAPA03 ( 0.80D0 ), 0.195088476224D-1, 1D-12,&
                 'FAPA03', ' ', STATUS )

      END

      SUBROUTINE T_FASA03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A S A 0 3
!  - - - - - - - - - - - - -
!
!  Test FASA03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FASA03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FASA03 ( 0.80D0 ),&
                 5.371574539440827046D0, 1D-12,&
                 'FASA03', ' ', STATUS )

      END

      SUBROUTINE T_FAUR03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A U R 0 3
!  - - - - - - - - - - - - -
!
!  Test FAUR03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAUR03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAUR03 ( 0.80D0 ),&
                 5.180636450180413523D0, 1D-12,&
                 'FAUR03', ' ', STATUS )

      END

      SUBROUTINE T_FAVE03 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F A V E 0 3
!  - - - - - - - - - - - - -
!
!  Test FAVE03 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FAVE03, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( FAVE03 ( 0.80D0 ),&
                 3.424900460533758000D0, 1D-12,&
                 'FAVE03', ' ', STATUS )

      END

      SUBROUTINE T_FK425 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 4 2 5
!  - - - - - - - - - - - -
!
!  Test FK425 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK425, VVD
!
!  This revision:  2018 December 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R1950, D1950, DR1950, DD1950, P1950, V1950,&
                       R2000, D2000, DR2000, DD2000, P2000, V2000


      R1950 = 0.07626899753879587532D0
      D1950 = -1.137405378399605780D0
      DR1950 = 0.1973749217849087460D-4
      DD1950 = 0.5659714913272723189D-5
      P1950 = 0.134D0
      V1950 = 8.7D0

      CALL FK425 ( R1950, D1950, DR1950, DD1950, P1950, V1950,&
                       R2000, D2000, DR2000, DD2000, P2000, V2000 )

      CALL VVD ( R2000, 0.08757989933556446040D0, 1D-14,&
                 'FK425', 'R2000', STATUS )
      CALL VVD ( D2000, -1.132279113042091895D0, 1D-12,&
                 'FK425', 'D2000', STATUS )
      CALL VVD ( DR2000, 0.1953670614474396139D-4, 1D-17,&
                 'FK425', 'DR2000', STATUS )
      CALL VVD ( DD2000, 0.5637686678659640164D-5, 1D-18,&
                 'FK425', 'DD2000', STATUS )
      CALL VVD ( P2000, 0.1339919950582767871D0, 1D-13,&
                 'FK425', 'P2000', STATUS )
      CALL VVD ( V2000, 8.736999669183529069D0, 1D-12,&
                 'FK425', 'V2000', STATUS )

      END

      SUBROUTINE T_FK45Z ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 4 5 Z
!  - - - - - - - - - - - -
!
!  Test FK45Z routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK45Z, VVD
!
!  This revision:  2018 December 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R1950, D1950, BEPOCH, R2000, D2000


      R1950 = 0.01602284975382960982D0
      D1950 = -0.1164347929099906024D0
      BEPOCH = 1954.677617625256806D0

      CALL FK45Z ( R1950, D1950, BEPOCH, R2000, D2000 )

      CALL VVD ( R2000, 0.02719295911606862303D0, 1D-15,&
                 'FK45Z', 'R2000', STATUS )
      CALL VVD ( D2000, -0.1115766001565926892D0, 1D-13,&
                 'FK45Z', 'D2000', STATUS )

      END

      SUBROUTINE T_FK524 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 5 2 4
!  - - - - - - - - - - - -
!
!  Test FK524 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK524, VVD
!
!  This revision:  2018 December 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R2000, D2000, DR2000, DD2000, P2000, V2000,&
                       R1950, D1950, DR1950, DD1950, P1950, V1950


      R2000 = 0.8723503576487275595D0
      D2000 = -0.7517076365138887672D0
      DR2000 = 0.2019447755430472323D-4
      DD2000 = 0.3541563940505160433D-5
      P2000 = 0.1559D0
      V2000 = 86.87D0

      CALL FK524 ( R2000, D2000, DR2000, DD2000, P2000, V2000,&
                       R1950, D1950, DR1950, DD1950, P1950, V1950 )

      CALL VVD ( R1950, 0.8636359659799603487D0, 1D-13,&
                 'FK524', 'R1950', STATUS )
      CALL VVD ( D1950, -0.7550281733160843059D0, 1D-13,&
                 'FK524', 'D1950', STATUS )
      CALL VVD ( DR1950, 0.2023628192747172486D-4, 1D-17,&
                 'FK524', 'DR1950', STATUS )
      CALL VVD ( DD1950, 0.3624459754935334718D-5, 1D-18,&
                 'FK524', 'DD1950', STATUS )
      CALL VVD ( P1950, 0.1560079963299390241D0, 1D-13,&
                 'FK524', 'P1950', STATUS )
      CALL VVD ( V1950, 86.79606353469163751D0, 1D-11,&
                 'FK524', 'V1950', STATUS )

      END

      SUBROUTINE T_FK52H ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 5 2 H
!  - - - - - - - - - - - -
!
!  Test FK52H routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK52H, VVD
!
!  This revision:  2017 January 3
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5, D5, DR5, DD5, PX5, RV5,&
                       RH, DH, DRH, DDH, PXH, RVH


      R5 = 1.76779433D0
      D5 = -0.2917517103D0
      DR5 = -1.91851572D-7
      DD5 = -5.8468475D-6
      PX5 = 0.379210D0
      RV5 = -7.6D0

      CALL FK52H ( R5, D5, DR5, DD5, PX5, RV5,&
                       RH, DH, DRH, DDH, PXH, RVH )

      CALL VVD ( RH, 1.767794226299947632D0, 1D-14,&
                 'FK52H', 'RA', STATUS )
      CALL VVD ( DH, -0.2917516070530391757D0, 1D-14,&
                 'FK52H', 'DEC', STATUS )
      CALL VVD ( DRH, -0.19618741256057224D-6, 1D-19,&
                 'FK52H', 'DR5', STATUS )
      CALL VVD ( DDH, -0.58459905176693911D-5, 1D-19,&
                 'FK52H', 'DD5', STATUS )
      CALL VVD ( PXH, 0.37921D0, 1D-14,&
                 'FK52H', 'PX', STATUS )
      CALL VVD ( RVH, -7.6000000940000254D0, 1D-11,&
                 'FK52H', 'RV', STATUS )

      END

      SUBROUTINE T_FK54Z ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 5 4 Z
!  - - - - - - - - - - - -
!
!  Test FK54Z routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK54Z, VVD
!
!  This revision:  2018 December 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R2000, D2000, BEPOCH,&
                       R1950, D1950, DR1950, DD1950


      R2000 = 0.02719026625066316119D0
      D2000 = -0.1115815170738754813D0
      BEPOCH = 1954.677308160316374D0

      CALL FK54Z ( R2000, D2000, BEPOCH,&
                       R1950, D1950, DR1950, DD1950 )

      CALL VVD ( R1950, 0.01602015588390065476D0, 1D-14,&
                 ' FK54Z', 'R1950', STATUS )
      CALL VVD ( D1950, -0.1164397101110765346D0, 1D-13,&
                 ' FK54Z', 'D1950', STATUS )
      CALL VVD ( DR1950, -0.1175712648471090704D-7, 1D-20,&
                 ' FK54Z', 'DR1950', STATUS )
      CALL VVD ( DD1950, 0.2108109051316431056D-7, 1D-20,&
                 ' FK54Z', 'DD1950', STATUS )

      END

      SUBROUTINE T_FK5HIP ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ F K 5 H I P
!  - - - - - - - - - - - - -
!
!  Test FK5HIP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK5HIP, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5H(3,3), S5H(3)


      CALL FK5HIP ( R5H, S5H )

      CALL VVD ( R5H(1,1), 0.9999999999999928638D0, 1D-14,&
                 'FK5HIP', 'R511', STATUS )
      CALL VVD ( R5H(1,2), 0.1110223351022919694D-6, 1D-17,&
                 'FK5HIP', 'R512', STATUS )
      CALL VVD ( R5H(1,3), 0.4411803962536558154D-7, 1D-17,&
                 'FK5HIP', 'R513', STATUS )
      CALL VVD ( R5H(2,1), -0.1110223308458746430D-6, 1D-17,&
                 'FK5HIP', 'R521', STATUS )
      CALL VVD ( R5H(2,2), 0.9999999999999891830D0, 1D-14,&
                 'FK5HIP', 'R522', STATUS )
      CALL VVD ( R5H(2,3), -0.9647792498984142358D-7, 1D-17,&
                 'FK5HIP', 'R523', STATUS )
      CALL VVD ( R5H(3,1), -0.4411805033656962252D-7, 1D-17,&
                 'FK5HIP', 'R531', STATUS )
      CALL VVD ( R5H(3,2), 0.9647792009175314354D-7, 1D-17,&
                 'FK5HIP', 'R532', STATUS )
      CALL VVD ( R5H(3,3), 0.9999999999999943728D0, 1D-14,&
                 'FK5HIP', 'R533', STATUS )
      CALL VVD ( S5H(1), -0.1454441043328607981D-8, 1D-17,&
                 'FK5HIP', 'S51', STATUS )
      CALL VVD ( S5H(2), 0.2908882086657215962D-8, 1D-17,&
                 'FK5HIP', 'S52', STATUS )
      CALL VVD ( S5H(3), 0.3393695767766751955D-8, 1D-17,&
                 'FK5HIP', 'S53', STATUS )

      END

      SUBROUTINE T_FK5HZ ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F K 5 H Z
!  - - - - - - - - - - - -
!
!  Test FK5HZ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FK5HZ, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5, D5, RH, DH


      R5 = 1.76779433D0
      D5 = -0.2917517103D0

      CALL FK5HZ ( R5, D5, 2400000.5D0, 54479D0, RH, DH )

      CALL VVD ( RH, 1.767794191464423978D0, 1D-12,&
                 'FK5HZ', 'RA', STATUS )
      CALL VVD ( DH, -0.2917516001679884419D0, 1D-12,&
                 'FK5HZ', 'DEC', STATUS )

      END

      SUBROUTINE T_FW2M ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ F W 2 M
!  - - - - - - - - - - -
!
!  Test FW2M routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FW2M, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSI, EPS, R(3,3)


      GAMB = -0.2243387670997992368D-5
      PHIB = 0.4091014602391312982D0
      PSI = -0.9501954178013015092D-3
      EPS = 0.4091014316587367472D0

      CALL FW2M ( GAMB, PHIB, PSI, EPS, R )

      CALL VVD ( R(1,1), 0.9999995505176007047D0, 1D-12,&
                 'FW2M', '11', STATUS )
      CALL VVD ( R(1,2), 0.8695404617348192957D-3, 1D-12,&
                 'FW2M', '12', STATUS )
      CALL VVD ( R(1,3), 0.3779735201865582571D-3, 1D-12,&
                 'FW2M', '13', STATUS )
      CALL VVD ( R(2,1), -0.8695404723772016038D-3, 1D-12,&
                 'FW2M', '21', STATUS )
      CALL VVD ( R(2,2), 0.9999996219496027161D0, 1D-12,&
                 'FW2M', '22', STATUS )
      CALL VVD ( R(2,3), -0.1361752496887100026D-6, 1D-12,&
                 'FW2M', '23', STATUS )
      CALL VVD ( R(3,1), -0.3779734957034082790D-3, 1D-12,&
                 'FW2M', '31', STATUS )
      CALL VVD ( R(3,2), -0.1924880848087615651D-6, 1D-12,&
                 'FW2M', '32', STATUS )
      CALL VVD ( R(3,3), 0.9999999285679971958D0, 1D-12,&
                 'FW2M', '33', STATUS )

      END

      SUBROUTINE T_FW2XY ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ F W 2 X Y
!  - - - - - - - - - - - -
!
!  Test FW2XY routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  FW2XY, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSI, EPS, X, Y


      GAMB = -0.2243387670997992368D-5
      PHIB = 0.4091014602391312982D0
      PSI = -0.9501954178013015092D-3
      EPS = 0.4091014316587367472D0

      CALL FW2XY ( GAMB, PHIB, PSI, EPS, X, Y )

      CALL VVD ( X, -0.3779734957034082790D-3, 1D-14,&
                 'FW2XY', 'X', STATUS )
      CALL VVD ( Y, -0.1924880848087615651D-6, 1D-14,&
                 'FW2XY', 'Y', STATUS )

      END

      SUBROUTINE T_G2ICRS ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G 2 I C R S
!  - - - - - - - - - - - - -
!
!  Test G2ICRS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  G2ICRS, VVD
!
!  This revision:  2015 January 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DL, DB, DR, DD


      DL =  5.5850536063818546461558105D0
      DB = -0.7853981633974483096156608D0

      CALL G2ICRS ( DL, DB, DR, DD )

      CALL VVD ( DR,  5.9338074302227188048671D0, 1D-14,&
                 'G2ICRS', 'R', STATUS )
      CALL VVD ( DD, -1.1784870613579944551541D0, 1D-14,&
                 'G2ICRS', 'D', STATUS )

      END

      SUBROUTINE T_GC2GD ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ G C 2 G D
!  - - - - - - - - - - - -
!
!  Test GC2GD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GC2GD, VVD, VIV
!
!  This revision:  2016 March 12
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION XYZ(3), E, P, H
      DATA XYZ / 2D6, 3D6, 5.244D6 /


      CALL GC2GD ( 0, XYZ, E, P, H, J )

      CALL VIV ( J, -1, 'GC2GD', 'J0', STATUS )

      CALL GC2GD( 1, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'GC2GD', 'J1', STATUS )
      CALL VVD ( E, 0.9827937232473290680D0, 1D-14,&
                 'GC2GD', 'E1', STATUS )
      CALL VVD ( P, 0.97160184819075459D0, 1D-14,&
                 'GC2GD', 'P1', STATUS )
      CALL VVD ( H, 331.4172461426059892D0, 1D-8,&
                 'GC2GD', 'H1', STATUS )

      CALL GC2GD ( 2, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'GC2GD', 'J2', STATUS )
      CALL VVD ( E, 0.9827937232473290680D0, 1D-14,&
                 'GC2GD', 'E2', STATUS )
      CALL VVD ( P, 0.97160184820607853D0, 1D-14,&
                 'GC2GD', 'P2', STATUS )
      CALL VVD ( H, 331.41731754844348D0, 1D-8,&
                 'GC2GD', 'H2', STATUS )

      CALL GC2GD ( 3, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'GC2GD', 'J3', STATUS )
      CALL VVD ( E, 0.9827937232473290680D0, 1D-14,&
                 'GC2GD', 'E3', STATUS )
      CALL VVD ( P, 0.9716018181101511937D0, 1D-14,&
                 'GC2GD', 'P3', STATUS )
      CALL VVD ( H, 333.2770726130318123D0, 1D-8,&
                 'GC2GD', 'H3', STATUS )

      CALL GC2GD ( 4, XYZ, E, P, H, J )

      CALL VIV ( J, -1, 'GC2GD', 'J4', STATUS )

      END

      SUBROUTINE T_GC2GDE ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G C 2 G D E
!  - - - - - - - - - - - - -
!
!  Test GC2GDE routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GC2GDE, VIV, VVD
!
!  This revision:  2016 March 12
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J

      DOUBLE PRECISION A, F, XYZ(3), E, P, H
      DATA A, F, XYZ / 6378136D0, 0.0033528D0, 2D6, 3D6, 5.244D6 /


      CALL GC2GDE ( a, f, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'GC2GDE', 'J', STATUS )
      CALL VVD ( E, 0.9827937232473290680D0, 1D-14,&
                 'GC2GDE', 'E', STATUS )
      CALL VVD ( P, 0.9716018377570411532D0, 1D-14,&
                 'GC2GDE', 'P', STATUS )
      CALL VVD ( H, 332.36862495764397D0, 1D-8,&
                 'GC2GDE', 'H', STATUS )

      END

      SUBROUTINE T_GD2GC ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ G D 2 G C
!  - - - - - - - - - - - -
!
!  Test GD2GC routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GD2GC, VIV, VVD
!
!  This revision:  2016 March 12
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION E, P, H, XYZ(3)
      DATA E, P, H / 3.1D0, -0.5D0, 2500D0 /


      CALL GD2GC ( 0, E, P, H, XYZ, J )

      CALL VIV ( J, -1, 'GD2GC', 'J0', STATUS )

      CALL GD2GC( 1, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'GD2GC', 'J1', STATUS )
      CALL VVD ( XYZ(1), -5599000.5577049947D0, 1D-7,&
                 'GD2GC', '1/1', STATUS )
      CALL VVD ( XYZ(2), 233011.67223479203D0, 1D-7,&
                 'GD2GC', '2/1', STATUS )
      CALL VVD ( XYZ(3), -3040909.4706983363D0, 1D-7,&
                 'GD2GC', '3/1', STATUS )

      CALL GD2GC( 2, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'GD2GC', 'J2', STATUS )
      CALL VVD ( XYZ(1), -5599000.5577260984D0, 1D-7,&
                 'GD2GC', '1/2', STATUS )
      CALL VVD ( XYZ(2), 233011.6722356702949D0, 1D-7,&
                 'GD2GC', '2/2', STATUS )
      CALL VVD ( XYZ(3), -3040909.4706095476D0, 1D-7,&
                 'GD2GC', '3/2', STATUS )

      CALL GD2GC( 3, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'GD2GC', 'J3', STATUS )
      CALL VVD ( XYZ(1), -5598998.7626301490D0, 1D-7,&
                 'GD2GC', '1/3', STATUS )
      CALL VVD ( XYZ(2), 233011.5975297822211D0, 1D-7,&
                 'GD2GC', '2/3', STATUS )
      CALL VVD ( XYZ(3), -3040908.6861467111D0, 1D-7,&
                 'GD2GC', '3/3', STATUS )

      CALL GD2GC( 4, E, P, H, XYZ, J )

      CALL VIV ( J, -1, 'GD2GC', 'J4', STATUS )

      END

      SUBROUTINE T_GD2GCE ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G D 2 G C E
!  - - - - - - - - - - - - -
!
!  Test GD2GCE routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GD2GCE, VIV, VVD
!
!  This revision:  2016 March 12
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION A, F, E, P, H, XYZ(3)
      DATA A, F, E, P, H / 6378136D0, 0.0033528D0,&
                           3.1D0, -0.5D0, 2500D0 /


      CALL GD2GCE ( A, F, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'GD2GCE', 'J', STATUS )
      CALL VVD ( XYZ(1), -5598999.6665116328D0, 1D-7,&
                 'GD2GCE', '1', STATUS )
      CALL VVD ( XYZ(2), 233011.6351463057189D0, 1D-7,&
                 'GD2GCE', '2', STATUS )
      CALL VVD ( XYZ(3), -3040909.0517314132D0, 1D-7,&
                 'GD2GCE', '3', STATUS )

      END

      SUBROUTINE T_GMST00 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G M S T 0 0
!  - - - - - - - - - - - - -
!
!  Test GMST00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GMST00, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GMST00 ( 2400000.5D0, 53736D0,&
                              2400000.5D0, 53736D0 ),&
                 1.754174972210740592D0, 1D-12,&
                 'GMST00', ' ', STATUS )

      END

      SUBROUTINE T_GMST06 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G M S T 0 6
!  - - - - - - - - - - - - -
!
!  Test GMST06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GMST06, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GMST06 ( 2400000.5D0, 53736D0,&
                              2400000.5D0, 53736D0 ),&
                 1.754174971870091203D0, 1D-12,&
                 'GMST06', ' ', STATUS )

      END

      SUBROUTINE T_GMST82 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G M S T 8 2
!  - - - - - - - - - - - - -
!
!  Test GMST82 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GMST82, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GMST82 ( 2400000.5D0, 53736D0 ),&
                 1.754174981860675096D0, 1D-12,&
                 'GMST82', ' ', STATUS )

      END

      SUBROUTINE T_GST00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G S T 0 0 A
!  - - - - - - - - - - - - -
!
!  Test GST00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GST00A, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GST00A ( 2400000.5D0, 53736D0,&
                              2400000.5D0, 53736D0 ),&
                 1.754166138018281369D0, 1D-12,&
                 'GST00A', ' ', STATUS )

      END

      SUBROUTINE T_GST00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G S T 0 0 B
!  - - - - - - - - - - - - -
!
!  Test GST00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GST00B, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GST00B ( 2400000.5D0, 53736D0 ),&
                 1.754166136510680589D0, 1D-12,&
                 'GST00B', ' ', STATUS )

      END

      SUBROUTINE T_GST06 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ G S T 0 6
!  - - - - - - - - - - - -
!
!  Test GST06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GST06, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RNPB(3,3)


      RNPB(1,1) = 0.9999989440476103608D0
      RNPB(1,2) = -0.1332881761240011518D-2
      RNPB(1,3) = -0.5790767434730085097D-3

      RNPB(2,1) = 0.1332858254308954453D-2
      RNPB(2,2) = 0.9999991109044505944D0
      RNPB(2,3) = -0.4097782710401555759D-4

      RNPB(3,1) = 0.5791308472168153320D-3
      RNPB(3,2) = 0.4020595661593994396D-4
      RNPB(3,3) = 0.9999998314954572365D0

      CALL VVD ( GST06 ( 2400000.5D0, 53736D0,&
                             2400000.5D0, 53736D0, RNPB ),&
                 1.754166138018167568D0, 1D-12,&
                 'GST06', ' ', STATUS )

      END

      SUBROUTINE T_GST06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ G S T 0 6 A
!  - - - - - - - - - - - - -
!
!  Test GST06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GST06A, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GST06A ( 2400000.5D0, 53736D0,&
                              2400000.5D0, 53736D0 ),&
                 1.754166137675019159D0, 1D-12,&
                 'GST06A', ' ', STATUS )

      END

      SUBROUTINE T_GST94 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ G S T 9 4
!  - - - - - - - - - - - -
!
!  Test GST94 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  GST94, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( GST94 ( 2400000.5D0, 53736D0 ),&
                 1.754166136020645203D0, 1D-12,&
                 'GST94', ' ', STATUS )

      END

      SUBROUTINE T_H2FK5 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ H 2 F K 5
!  - - - - - - - - - - - -
!
!  Test H2FK5 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  H2FK5, VVD
!
!  This revision:  2017 January 3
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RH, DH, DRH, DDH, PXH, RVH,&
                       R5, D5, DR5, DD5, PX5, RV5


      RH = 1.767794352D0
      DH = -0.2917512594D0
      DRH = -2.76413026D-6
      DDH = -5.92994449D-6
      PXH = 0.379210D0
      RVH = -7.6D0

      CALL H2FK5 ( RH, DH, DRH, DDH, PXH, RVH,&
                       R5, D5, DR5, DD5, PX5, RV5 )

      CALL VVD ( R5, 1.767794455700065506D0, 1D-13,&
                 'H2FK5', 'RA', STATUS )
      CALL VVD ( D5, -0.2917513626469638890D0, 1D-13,&
                 'H2FK5', 'DEC', STATUS )
      CALL VVD ( DR5, -0.27597945024511204D-5, 1D-18,&
                 'H2FK5', 'DR5', STATUS )
      CALL VVD ( DD5, -0.59308014093262838D-5, 1D-18,&
                 'H2FK5', 'DD5', STATUS )
      CALL VVD ( PX5, 0.37921D0, 1D-13,&
                 'H2FK5', 'PX', STATUS )
      CALL VVD ( RV5, -7.6000001309071126D0, 1D-11,&
                 'H2FK5', 'RV', STATUS )

      END

      SUBROUTINE T_HD2AE ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ H D 2 A E
!  - - - - - - - - - - - -
!
!  Test HD2AE routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  HD2AE, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION H, D, P, A, E


      H = 1.1D0
      D = 1.2D0
      P = 0.3D0

      CALL HD2AE ( H, D, P, A, E )

      CALL VVD ( A, 5.916889243730066194D0, 1D-13,&
                 'HD2AE', 'A', STATUS )
      CALL VVD ( E, 0.4472186304990486228D0, 1D-14,&
                 'HD2AE', 'E', STATUS )

      END

      SUBROUTINE T_HD2PA ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ H D 2 P A
!  - - - - - - - - - - - -
!
!  Test HD2PA routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  HD2AE, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION H, D, P, Q


      H = 1.1D0
      D = 1.2D0
      P = 0.3D0

      Q = HD2PA ( H, D, P )

      CALL VVD ( Q, 1.906227428001995580D0, 1D-13,&
                 'HD2PA', 'Q', STATUS )

      END

      SUBROUTINE T_HFK5Z ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ H F K 5 Z
!  - - - - - - - - - - - -
!
!  Test HFK5Z routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  HFK5Z, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RH, DH, R5, D5, DR5, DD5


      RH = 1.767794352D0
      DH = -0.2917512594D0

      CALL HFK5Z ( RH, DH, 2400000.5D0, 54479D0,&
                       R5, D5, DR5, DD5 )

      CALL VVD ( R5, 1.767794490535581026D0, 1D-13,&
                 'HFK5Z', 'RA', STATUS )
      CALL VVD ( D5, -0.2917513695320114258D0, 1D-14,&
                 'HFK5Z', 'DEC', STATUS )
      CALL VVD ( DR5, 0.4335890983539243029D-8, 1D-22,&
                 'HFK5Z', 'DR5', STATUS )
      CALL VVD ( DD5, -0.8569648841237745902D-9, 1D-23,&
                 'HFK5Z', 'DD5', STATUS )

      END

      SUBROUTINE T_ICRS2G ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ I C R S 2 G
!  - - - - - - - - - - - - -
!
!  Test ICRS2G routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ICRS2G, VVD
!
!  This revision:  2015 January 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DR, DD, DL, DB


      DR =  5.9338074302227188048671087D0
      DD = -1.1784870613579944551540570D0

      CALL ICRS2G ( DR, DD, DL, DB )

      CALL VVD ( DL,  5.5850536063818546461558D0, 1D-14,&
                 'ICRS2G', 'L', STATUS )
      CALL VVD ( DB, -0.7853981633974483096157D0, 1D-14,&
                 'ICRS2G', 'B', STATUS )

      END

      SUBROUTINE T_IR ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ I R
!  - - - - - - - - -
!
!  Test IR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  IR, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  R(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL IR ( R )

      CALL VVD ( R(1,1), 1D0, 0D0, 'IR', '11', STATUS )
      CALL VVD ( R(1,2), 0D0, 0D0, 'IR', '12', STATUS )
      CALL VVD ( R(1,3), 0D0, 0D0, 'IR', '13', STATUS )
      CALL VVD ( R(2,1), 0D0, 0D0, 'IR', '21', STATUS )
      CALL VVD ( R(2,2), 1D0, 0D0, 'IR', '22', STATUS )
      CALL VVD ( R(2,3), 0D0, 0D0, 'IR', '23', STATUS )
      CALL VVD ( R(3,1), 0D0, 0D0, 'IR', '31', STATUS )
      CALL VVD ( R(3,2), 0D0, 0D0, 'IR', '32', STATUS )
      CALL VVD ( R(3,3), 1D0, 0D0, 'IR', '33', STATUS )

      END

      SUBROUTINE T_JD2CAL ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ J D 2 C A L
!  - - - - - - - - - - - - -
!
!  Test JD2CAL routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  JD2CAL, VIV, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J


      DJ1 = 2400000.5D0
      DJ2 = 50123.9999D0

      CALL JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )

      CALL VIV ( IY, 1996, 'JD2CAL', 'Y', STATUS )
      CALL VIV ( IM, 2, 'JD2CAL', 'M', STATUS )
      CALL VIV ( ID, 10, 'JD2CAL', 'D', STATUS )
      CALL VVD ( FD, 0.9999D0, 1D-7, 'JD2CAL', 'FD', STATUS )
      CALL VIV ( J, 0, 'JD2CAL', 'J', STATUS )

      END

      SUBROUTINE T_JDCALF ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ J D C A L F
!  - - - - - - - - - - - - -
!
!  Test JDCALF routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  JDCALF, VIV
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IYDMF(4)
      INTEGER J


      DJ1 = 2400000.5D0
      DJ2 = 50123.9999D0

      CALL JDCALF ( 4, DJ1, DJ2, IYDMF, J )

      CALL VIV ( IYDMF(1), 1996, 'JDCALF', 'Y', STATUS )
      CALL VIV ( IYDMF(2), 2, 'JDCALF', 'M', STATUS )
      CALL VIV ( IYDMF(3), 10, 'JDCALF', 'D', STATUS )
      CALL VIV ( IYDMF(4), 9999, 'JDCALF', 'F', STATUS )
      CALL VIV ( J, 0, 'JDCALF', 'J', STATUS )

      END

      SUBROUTINE T_LD ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ L D
!  - - - - - - - - -
!
!  Test LD routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LD, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION BM, P(3), Q(3), E(3), EM, DLIM, P1(3)


      BM = 0.00028574D0
      P(1) = -0.763276255D0
      P(2) = -0.608633767D0
      P(3) = -0.216735543D0
      Q(1) = -0.763276255D0
      Q(2) = -0.608633767D0
      Q(3) = -0.216735543D0
      E(1) = 0.76700421D0
      E(2) = 0.605629598D0
      E(3) = 0.211937094D0
      EM = 8.91276983D0
      DLIM = 3D-10

      CALL LD ( BM, P, Q, E, EM, DLIM, P1 )

      CALL VVD ( P1(1), -0.7632762548968159627D0, 1D-12,&
                 'LD', '1', STATUS )
      CALL VVD ( P1(2), -0.6086337670823762701D0, 1D-12,&
                 'LD', '2', STATUS )
      CALL VVD ( P1(3), -0.2167355431320546947D0, 1D-12,&
                 'LD', '3', STATUS )

      END

      SUBROUTINE T_LDN ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ L D N
!  - - - - - - - - - -
!
!  Test LDN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LDN, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER N
      DOUBLE PRECISION B(8,3), OB(3), SC(3), SN(3)


      N = 3
      B(1,1) =  0.00028574D0
      B(2,1) =  3D-10
      B(3,1) = -7.81014427D0
      B(4,1) = -5.60956681D0
      B(5,1) = -1.98079819D0
      B(6,1) =  0.0030723249D0
      B(7,1) = -0.00406995477D0
      B(8,1) = -0.00181335842D0
      B(1,2) =  0.00095435D0
      B(2,2) =  3D-9
      B(3,2) =  0.738098796D0
      B(4,2) =  4.63658692D0
      B(5,2) =  1.9693136D0
      B(6,2) = -0.00755816922D0
      B(7,2) =  0.00126913722D0
      B(8,2) =  0.000727999001D0
      B(1,3) =  1D0
      B(2,3) =  6D-6
      B(3,3) = -0.000712174377D0
      B(4,3) = -0.00230478303D0
      B(5,3) = -0.00105865966D0
      B(6,3) =  6.29235213D-6
      B(7,3) = -3.30888387D-7
      B(8,3) = -2.96486623D-7
      OB(1) =  -0.974170437D0
      OB(2) =  -0.2115201D0
      OB(3) =  -0.0917583114D0
      SC(1) =  -0.763276255D0
      SC(2) =  -0.608633767D0
      SC(3) =  -0.216735543D0

      CALL LDN ( N, B, OB, SC, SN )

      CALL VVD ( SN(1), -0.7632762579693333866D0, 1D-12,&
                 'LDN', '1', STATUS )
      CALL VVD ( SN(2), -0.6086337636093002660D0, 1D-12,&
                 'LDN', '2', STATUS )
      CALL VVD ( SN(3), -0.2167355420646328159D0, 1D-12,&
                 'LDN', '3', STATUS )

      END

      SUBROUTINE T_LDSUN ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ L D S U N
!  - - - - - - - - - - - -
!
!  Test LDSUN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LDSUN, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), E(3), EM, P1(3)


      P(1) = -0.763276255D0
      P(2) = -0.608633767D0
      P(3) = -0.216735543D0
      E(1) = -0.973644023D0
      E(2) = -0.20925523D0
      E(3) = -0.0907169552D0
      EM = 0.999809214D0

      CALL LDSUN ( P, E, EM, P1 )

      CALL VVD ( P1(1), -0.7632762580731413169D0, 1D-12,&
                 'LDSUN', '1', STATUS )
      CALL VVD ( P1(2), -0.6086337635262647900D0, 1D-12,&
                 'LDSUN', '2', STATUS )
      CALL VVD ( P1(3), -0.2167355419322321302D0, 1D-12,&
                 'LDSUN', '3', STATUS )

      END

      SUBROUTINE T_LTECEQ ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ L T E C E Q
!  - - - - - - - - - - - - -
!
!  Test LTECEQ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTECEQ, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, DL, DB, DR, DD


      EPJ = 2500D0
      DL = 1.5D0
      DB = 0.6D0

      CALL LTECEQ ( EPJ, DL, DB, DR, DD )

      CALL VVD ( DR, 1.275156021861921167D0, 1D-14,&
                 'LTECEQ', 'DR', STATUS )
      CALL VVD ( DD, 0.9966573543519204791D0, 1D-14,&
                 'LTECEQ', 'DD', STATUS )

      END

      SUBROUTINE T_LTECM ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ L T E C M
!  - - - - - - - - - - - -
!
!  Test LTECM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTECM, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, RM(3,3)


      EPJ = -3000D0

      CALL LTECM ( EPJ, RM )

      CALL VVD ( RM(1,1), 0.3564105644859788825D0, 1D-14,&
                 'LTECM', 'RM11', STATUS )
      CALL VVD ( RM(1,2), 0.8530575738617682284D0, 1D-14,&
                 'LTECM', 'RM12', STATUS )
      CALL VVD ( RM(1,3), 0.3811355207795060435D0, 1D-14,&
                 'LTECM', 'RM13', STATUS )
      CALL VVD ( RM(2,1), -0.9343283469640709942D0, 1D-14,&
                 'LTECM', 'RM21', STATUS )
      CALL VVD ( RM(2,2), 0.3247830597681745976D0, 1D-14,&
                 'LTECM', 'RM22', STATUS )
      CALL VVD ( RM(2,3), 0.1467872751535940865D0, 1D-14,&
                 'LTECM', 'RM23', STATUS )
      CALL VVD ( RM(3,1), 0.1431636191201167793D-2, 1D-14,&
                 'LTECM', 'RM31', STATUS )
      CALL VVD ( RM(3,2), -0.4084222566960599342D0, 1D-14,&
                 'LTECM', 'RM32', STATUS )
      CALL VVD ( RM(3,3), 0.9127919865189030899D0, 1D-14,&
                 'LTECM', 'RM33', STATUS )

      END

      SUBROUTINE T_LTEQEC ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ L T E Q E C
!  - - - - - - - - - - - - -
!
!  Test LTEQEC routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTEQEC, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, DR, DD, DL, DB


      EPJ = -1500D0
      DR = 1.234D0
      DD = 0.987D0

      CALL LTEQEC ( EPJ, DR, DD, DL, DB )

      CALL VVD ( DL, 0.5039483649047114859D0, 1D-14,&
                 'LTEQEC', 'DL', STATUS )
      CALL VVD ( DB, 0.5848534459726224882D0, 1D-14,&
                 'LTEQEC', 'DB', STATUS )

      END

      SUBROUTINE T_LTP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ L T P
!  - - - - - - - - - -
!
!  Test LTP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTP, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, RP(3,3)


      EPJ = 1666.666D0

      CALL LTP ( EPJ, RP )

      CALL VVD ( RP(1,1), 0.9967044141159213819D0, 1D-14,&
                 'LTP', 'RP11', STATUS )
      CALL VVD ( RP(1,2), 0.7437801893193210840D-1, 1D-14,&
                 'LTP', 'RP12', STATUS )
      CALL VVD ( RP(1,3), 0.3237624409345603401D-1, 1D-14,&
                 'LTP', 'RP13', STATUS )
      CALL VVD ( RP(2,1), -0.7437802731819618167D-1, 1D-14,&
                 'LTP', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9972293894454533070D0, 1D-14,&
                 'LTP', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.1205768842723593346D-2, 1D-14,&
                 'LTP', 'RP23', STATUS )
      CALL VVD ( RP(3,1), -0.3237622482766575399D-1, 1D-14,&
                 'LTP', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.1206286039697609008D-2, 1D-14,&
                 'LTP', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9994750246704010914D0, 1D-14,&
                 'LTP', 'RP33', STATUS )

      END

      SUBROUTINE T_LTPB ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ L T P B
!  - - - - - - - - - - -
!
!  Test LTPB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTPB, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, RPB(3,3)


      EPJ = 1666.666D0

      CALL LTPB ( EPJ, RPB )

      CALL VVD ( RPB(1,1), 0.9967044167723271851D0, 1D-14,&
                 'LTPB', 'RPB11', STATUS )
      CALL VVD ( RPB(1,2), 0.7437794731203340345D-1, 1D-14,&
                 'LTPB', 'RPB12', STATUS )
      CALL VVD ( RPB(1,3), 0.3237632684841625547D-1, 1D-14,&
                 'LTPB', 'RPB13', STATUS )
      CALL VVD ( RPB(2,1), -0.7437795663437177152D-1, 1D-14,&
                 'LTPB', 'RPB21', STATUS )
      CALL VVD ( RPB(2,2), 0.9972293947500013666D0, 1D-14,&
                 'LTPB', 'RPB22', STATUS )
      CALL VVD ( RPB(2,3), -0.1205741865911243235D-2, 1D-14,&
                 'LTPB', 'RPB23', STATUS )
      CALL VVD ( RPB(3,1), -0.3237630543224664992D-1, 1D-14,&
                 'LTPB', 'RPB31', STATUS )
      CALL VVD ( RPB(3,2), -0.1206316791076485295D-2, 1D-14,&
                 'LTPB', 'RPB32', STATUS )
      CALL VVD ( RPB(3,3), 0.9994750220222438819D0, 1D-14,&
                 'LTPB', 'RPB33', STATUS )

      END

      SUBROUTINE T_LTPECL ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ L T P E C L
!  - - - - - - - - - - - - -
!
!  Test LTPECL routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTPECL, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, VEC(3)


      EPJ = -1500D0

      CALL LTPECL ( EPJ, VEC )

      CALL VVD ( VEC(1), 0.4768625676477096525D-3, 1D-14,&
                 'LTPECL', 'VEC1', STATUS )
      CALL VVD ( VEC(2), -0.4052259533091875112D0, 1D-14,&
                 'LTPECL', 'VEC2', STATUS )
      CALL VVD ( VEC(3), 0.9142164401096448012D0, 1D-14,&
                 'LTPECL', 'VEC3', STATUS )

      END

      SUBROUTINE T_LTPEQU ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ L T P E Q U
!  - - - - - - - - - - - - -
!
!  Test LTPEQU routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  LTPEQU, VVD
!
!  This revision:  2016 March 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, VEQ(3)


      EPJ = -2500D0

      CALL LTPEQU ( EPJ, VEQ )

      CALL VVD ( VEQ(1), -0.3586652560237326659D0, 1D-14,&
                 'LTPEQU', 'VEQ1', STATUS )
      CALL VVD ( VEQ(2), -0.1996978910771128475D0, 1D-14,&
                 'LTPEQU', 'VEQ2', STATUS )
      CALL VVD ( VEQ(3), 0.9118552442250819624D0, 1D-14,&
                 'LTPEQU', 'VEQ3', STATUS )

      END

      SUBROUTINE T_NUM00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U M 0 0 A
!  - - - - - - - - - - - - -
!
!  Test NUM00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUM00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL NUM00A ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227949D0, 1D-12,&
                 'NUM00A', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836238544090873336D-5, 1D-12,&
                 'NUM00A', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830835237722400669D-5, 1D-12,&
                 'NUM00A', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836082880798569274D-5, 1D-12,&
                 'NUM00A', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354655028D0, 1D-12,&
                 'NUM00A', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240865362499850D-4, 1D-12,&
                 'NUM00A', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831194272065995866D-5, 1D-12,&
                 'NUM00A', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063237480216291775D-4, 1D-12,&
                 'NUM00A', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671660338D0, 1D-12,&
                 'NUM00A', '33', STATUS )

      END

      SUBROUTINE T_NUM00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U M 0 0 B
!  - - - - - - - - - - - - -
!
!  Test NUM00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUM00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL NUM00B ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536069682D0, 1D-12,&
                 'NUM00B', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8837746144871248011D-5, 1D-12,&
                 'NUM00B', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3831488838252202945D-5, 1D-12,&
                 'NUM00B', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8837590456632304720D-5, 1D-12,&
                 'NUM00B', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354692733D0, 1D-12,&
                 'NUM00B', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063198798559591654D-4, 1D-12,&
                 'NUM00B', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831847930134941271D-5, 1D-12,&
                 'NUM00B', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063195412258168380D-4, 1D-12,&
                 'NUM00B', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671806225D0, 1D-12,&
                 'NUM00B', '33', STATUS )

      END

      SUBROUTINE T_NUM06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U M 0 6 A
!  - - - - - - - - - - - - -
!
!  Test NUM06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUM06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL NUM06A ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227668D0, 1D-12,&
                 'NUM06A', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836241998111535233D-5, 1D-12,&
                 'NUM06A', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830834608415287707D-5, 1D-12,&
                 'NUM06A', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836086334870740138D-5, 1D-12,&
                 'NUM06A', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354657474D0, 1D-12,&
                 'NUM06A', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240188248455065D-4, 1D-12,&
                 'NUM06A', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831193642839398128D-5, 1D-12,&
                 'NUM06A', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063236803101479770D-4, 1D-12,&
                 'NUM06A', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671663114D0, 1D-12,&
                 'NUM06A', '33', STATUS )

      END

      SUBROUTINE T_NUMAT ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ N U M A T
!  - - - - - - - - - - - -
!
!  Test NUMAT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUMAT, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPSA, DPSI, DEPS, RMATN(3,3)


      EPSA = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5
      DEPS = 0.4063239174001678826D-4

      CALL NUMAT ( EPSA, DPSI, DEPS, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227949D0, 1D-12,&
                 'NUMAT', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836239320236250577D-5, 1D-12,&
                 'NUMAT', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830833447458251908D-5, 1D-12,&
                 'NUMAT', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836083657016688588D-5, 1D-12,&
                 'NUMAT', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354654959D0, 1D-12,&
                 'NUMAT', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240865361857698D-4, 1D-12,&
                 'NUMAT', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831192481833385226D-5, 1D-12,&
                 'NUMAT', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063237480216934159D-4, 1D-12,&
                 'NUMAT', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671660407D0, 1D-12,&
                 'NUMAT', '33', STATUS )

      END

      SUBROUTINE T_NUT00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U T 0 0 A
!  - - - - - - - - - - - - -
!
!  Test NUT00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUT00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL NUT00A ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9630909107115518431D-5, 1D-13,&
                 'NUT00A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063239174001678710D-4, 1D-13,&
                 'NUT00A', 'DEPS', STATUS )

      END

      SUBROUTINE T_NUT00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U T 0 0 B
!  - - - - - - - - - - - - -
!
!  Test NUT00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUT00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL NUT00B ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9632552291148362783D-5, 1D-13,&
                 'NUT00B', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063197106621159367D-4, 1D-13,&
                 'NUT00B', 'DEPS', STATUS )

      END

      SUBROUTINE T_NUT06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U T 0 6 A
!  - - - - - - - - - - - - -
!
!  Test NUT06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUT06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL NUT06A ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9630912025820308797D-5, 1D-13,&
                 'NUT06A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063238496887249798D-4, 1D-13,&
                 'NUT06A', 'DEPS', STATUS )

      END

      SUBROUTINE T_NUT80 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ N U T 8 0
!  - - - - - - - - - - - -
!
!  Test NUT80 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUT80, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL NUT80 ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9643658353226563966D-5, 1D-13,&
                 'NUT80', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4060051006879713322D-4, 1D-13,&
                 'NUT80', 'DEPS', STATUS )

      END

      SUBROUTINE T_NUTM80 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ N U T M 8 0
!  - - - - - - - - - - - - -
!
!  Test NUTM80 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  NUTM80, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL NUTM80 ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999534999268D0, 1D-12,&
                 'NUTM80', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8847935789636432161D-5, 1D-12,&
                 'NUTM80', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3835906502164019142D-5, 1D-12,&
                 'NUTM80', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8847780042583435924D-5, 1D-12,&
                 'NUTM80', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991366569963D0, 1D-12,&
                 'NUTM80', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4060052702727130809D-4, 1D-12,&
                 'NUTM80', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3836265729708478796D-5, 1D-12,&
                 'NUTM80', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4060049308612638555D-4, 1D-12,&
                 'NUTM80', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991684415129D0, 1D-12,&
                 'NUTM80', '33', STATUS )

      END

      SUBROUTINE T_OBL06 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ O B L 0 6
!  - - - - - - - - - - - -
!
!  Test OBL06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  OBL06, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( OBL06 ( 2400000.5D0, 54388D0 ),&
                 0.4090749229387258204D0, 1D-14,&
                 'OBL06', ' ', STATUS )

      END

      SUBROUTINE T_OBL80 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ O B L 8 0
!  - - - - - - - - - - - -
!
!  Test OBL80 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  OBL80, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( OBL80 ( 2400000.5D0, 54388D0 ),&
                 0.4090751347643816218D0, 1D-14,&
                 'OBL06', ' ', STATUS )

      END

      SUBROUTINE T_P06E ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P 0 6 E
!  - - - - - - - - - - -
!
!  Test P06E routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  P06E, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPS0, PSIA, OMA, BPA, BQA, PIA, BPIA,&
                       EPSA, CHIA, ZA, ZETAA, THETAA, PA,&
                       GAM, PHI, PSI


      CALL P06E ( 2400000.5D0, 52541D0, EPS0, PSIA, OMA, BPA,&
                      BQA, PIA, BPIA, EPSA, CHIA, ZA,&
                      ZETAA, THETAA, PA, GAM, PHI, PSI )

      CALL VVD ( EPS0, 0.4090926006005828715D0, 1D-14,&
                 'P06E', 'EPS0', STATUS )
      CALL VVD ( PSIA, 0.6664369630191613431D-3, 1D-14,&
                 'P06E', 'PSIA', STATUS )
      CALL VVD ( OMA, 0.4090925973783255982D0, 1D-14,&
                 'P06E', 'OMA', STATUS )
      CALL VVD ( BPA, 0.5561149371265209445D-6, 1D-14,&
                 'P06E', 'BPA', STATUS )
      CALL VVD ( BQA, -0.6191517193290621270D-5, 1D-14,&
                 'P06E', 'BQA', STATUS )
      CALL VVD ( PIA, 0.6216441751884382923D-5, 1D-14,&
                 'P06E', 'PIA', STATUS )
      CALL VVD ( BPIA, 3.052014180023779882D0, 1D-14,&
                 'P06E', 'BPIA', STATUS )
      CALL VVD ( EPSA, 0.4090864054922431688D0, 1D-14,&
                 'P06E', 'EPSA', STATUS )
      CALL VVD ( CHIA, 0.1387703379530915364D-5, 1D-14,&
                 'P06E', 'CHIA', STATUS )
      CALL VVD ( ZA, 0.2921789846651790546D-3, 1D-14,&
                 'P06E', 'ZA', STATUS )
      CALL VVD ( ZETAA, 0.3178773290332009310D-3, 1D-14,&
                 'P06E', 'ZETAA', STATUS )
      CALL VVD ( THETAA, 0.2650932701657497181D-3, 1D-14,&
                 'P06E', 'THETAA', STATUS )
      CALL VVD ( PA, 0.6651637681381016344D-3, 1D-14,&
                 'P06E', 'PA', STATUS )
      CALL VVD ( GAM, 0.1398077115963754987D-5, 1D-14,&
                 'P06E', 'GAM', STATUS )
      CALL VVD ( PHI, 0.4090864090837462602D0, 1D-14,&
                 'P06E', 'PHI', STATUS )
      CALL VVD ( PSI, 0.6664464807480920325D-3, 1D-14,&
                 'P06E', 'PSI', STATUS )

      END

      SUBROUTINE T_P2PV ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P 2 P V
!  - - - - - - - - - - -
!
!  Test P2PV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  P2PV, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), PV(3,2)


      P(1) = 0.25D0
      P(2) = 1.2D0
      P(3) = 3D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL P2PV ( P, PV )

      CALL VVD ( PV(1,1), 0.25D0, 0D0, 'P2PV', 'P1', STATUS )
      CALL VVD ( PV(2,1), 1.2D0, 0D0, 'P2PV', 'P2', STATUS )
      CALL VVD ( PV(3,1), 3D0, 0D0, 'P2PV', 'P3', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0, 'P2PV', 'V1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0, 'P2PV', 'V2', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0, 'P2PV', 'V3', STATUS )

      END

      SUBROUTINE T_P2S ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P 2 S
!  - - - - - - - - - -
!
!  Test P2S routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  P2S, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), THETA, PHI, R


      P(1) = 100D0
      P(2) = -50D0
      P(3) = 25D0

      CALL P2S ( P, THETA, PHI, R )

      CALL VVD ( THETA, -0.4636476090008061162D0, 1D-12,&
                 'P2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.2199879773954594463D0, 1D-12,&
                 'P2S', 'PHI', STATUS )
      CALL VVD ( R, 114.5643923738960002D0, 1D-9,&
                 'P2S', 'R', STATUS )

      END

      SUBROUTINE T_PAP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P A P
!  - - - - - - - - - -
!
!  Test PAP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PAP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), THETA


      A(1) = 1D0
      A(2) = 0.1D0
      A(3) = 0.2D0

      B(1) = -3D0
      B(2) = 1D-3
      B(3) = 0.2D0

      CALL PAP ( A, B, THETA )

      CALL VVD ( THETA, 0.3671514267841113674D0, 1D-12,&
                 'PAP', ' ', STATUS )

      END

      SUBROUTINE T_PAS ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P A S
!  - - - - - - - - - -
!
!  Test PAS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PAS, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION AL, AP, BL, BP, THETA


      AL = 1D0
      AP = 0.1D0

      BL = 0.2D0
      BP = -1D0

      CALL PAS ( AL, AP, BL, BP, THETA )

      CALL VVD ( THETA, -2.724544922932270424D0, 1D-12,&
                 'PAS', ' ', STATUS )

      END

      SUBROUTINE T_PB06 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P B 0 6
!  - - - - - - - - - - -
!
!  Test PB06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PB06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION BZETA, BZ, BTHETA


      CALL PB06 ( 2400000.5D0, 50123.9999D0, BZETA, BZ, BTHETA )

      CALL VVD ( BZETA, -0.5092634016326478238D-3, 1D-12,&
                 'PB06', 'BZETA', STATUS )
      CALL VVD ( BZ, -0.3602772060566044413D-3, 1D-12,&
                 'PB06', 'BZ', STATUS )
      CALL VVD ( BTHETA, -0.3779735537167811177D-3, 1D-12,&
                 'PB06', 'BTHETA', STATUS )

      END

      SUBROUTINE T_PDP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P D P
!  - - - - - - - - - -
!
!  Test PDP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PDP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), ADB


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL PDP ( A, B, ADB )
      CALL VVD ( ADB, 20D0, 1D-12, 'PDP', ' ', STATUS )

      END

      SUBROUTINE T_PFW06 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P F W 0 6
!  - - - - - - - - - - - -
!
!  Test PFW06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PFW06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSIB, EPSA


      CALL PFW06 ( 2400000.5D0, 50123.9999D0,&
                       GAMB, PHIB, PSIB, EPSA )

      CALL VVD ( GAMB, -0.2243387670997995690D-5, 1D-16,&
                 'PFW06', 'GAMB', STATUS )
      CALL VVD ( PHIB, 0.4091014602391312808D0, 1D-12,&
                 'PFW06', 'PHIB', STATUS )
      CALL VVD ( PSIB, -0.9501954178013031895D-3, 1D-14,&
                 'PFW06', 'PSIB', STATUS )
      CALL VVD ( EPSA, 0.4091014316587367491D0, 1D-12,&
                 'PFW06', 'EPSA', STATUS )

      END

      SUBROUTINE T_PLAN94 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P L A N 9 4
!  - - - - - - - - - - - - -
!
!  Test PLAN94 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PLAN94, VVD, VIV
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)
      INTEGER J


      CALL PLAN94 ( 2400000.5D0, 1D6, 0, PV, J )

      CALL VVD ( PV(1,1), 0D0, 0D0,&
                 'PLAN94', 'PV(1) 1', STATUS )
      CALL VVD ( PV(2,1), 0D0, 0D0,&
                 'PLAN94', 'PV(2) 1', STATUS )
      CALL VVD ( PV(3,1), 0D0, 0D0,&
                 'PLAN94', 'PV(3) 1', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0,&
                 'PLAN94', 'PV(4) 1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0,&
                 'PLAN94', 'PV(5) 1', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0,&
                 'PLAN94', 'PV(6) 1', STATUS )
      CALL VIV ( J, -1, 'PLAN94', 'J 1', STATUS )

      CALL PLAN94 ( 2400000.5D0, 1D6, 10, PV, J )

      CALL VIV ( J, -1, 'PLAN94', 'J 2', STATUS )

      CALL PLAN94 ( 2400000.5D0, -320000D0, 3, PV, J )

      CALL VVD ( PV(1,1), 0.9308038666832975759D0, 1D-11,&
                 'PLAN94', 'PV(1) 3', STATUS )
      CALL VVD ( PV(2,1), 0.3258319040261346000D0, 1D-11,&
                 'PLAN94', 'PV(2) 3', STATUS )
      CALL VVD ( PV(3,1), 0.1422794544481140560D0, 1D-11,&
                 'PLAN94', 'PV(3) 3', STATUS )
      CALL VVD ( PV(1,2), -0.6429458958255170006D-2, 1D-11,&
                 'PLAN94', 'PV(4) 3', STATUS )
      CALL VVD ( PV(2,2), 0.1468570657704237764D-1, 1D-11,&
                 'PLAN94', 'PV(5) 3', STATUS )
      CALL VVD ( PV(3,2), 0.6406996426270981189D-2, 1D-11,&
                 'PLAN94', 'PV(6) 3', STATUS )
      CALL VIV ( J, 1, 'PLAN94', 'J 3', STATUS )

      CALL PLAN94 ( 2400000.5D0, 43999.9D0, 1, PV, J )

      CALL VVD ( PV(1,1), 0.2945293959257430832D0, 1D-11,&
                 'PLAN94', 'PV(1) 4', STATUS )
      CALL VVD ( PV(2,1), -0.2452204176601049596D0, 1D-11,&
                 'PLAN94', 'PV(2) 4', STATUS )
      CALL VVD ( PV(3,1), -0.1615427700571978153D0, 1D-11,&
                 'PLAN94', 'PV(3) 4', STATUS )
      CALL VVD ( PV(1,2), 0.1413867871404614441D-1, 1D-11,&
                 'PLAN94', 'PV(4) 4', STATUS )
      CALL VVD ( PV(2,2), 0.1946548301104706582D-1, 1D-11,&
                 'PLAN94', 'PV(5) 4', STATUS )
      CALL VVD ( PV(3,2), 0.8929809783898904786D-2, 1D-11,&
                 'PLAN94', 'PV(6) 4', STATUS )
      CALL VIV ( J, 0, 'PLAN94', 'J 4', STATUS )

      END

      SUBROUTINE T_PMAT00 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P M A T 0 0
!  - - - - - - - - - - - - -
!
!  Test PMAT00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMAT00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBP(3,3)


      CALL PMAT00 ( 2400000.5D0, 50123.9999D0, RBP )

      CALL VVD ( RBP(1,1), 0.9999995505175087260D0, 1D-12,&
                 'PMAT00', '11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695405883617884705D-3, 1D-14,&
                 'PMAT00', '12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779734722239007105D-3, 1D-14,&
                 'PMAT00', '13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695405990410863719D-3, 1D-14,&
                 'PMAT00', '21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219494925900D0, 1D-12,&
                 'PMAT00', '22', STATUS )
      CALL VVD ( RBP(2,3), -0.1360775820404982209D-6, 1D-14,&
                 'PMAT00', '23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734476558184991D-3, 1D-14,&
                 'PMAT00', '31', STATUS )
      CALL VVD ( RBP(3,2), -0.1925857585832024058D-6, 1D-14,&
                 'PMAT00', '32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285680153377D0, 1D-12,&
                 'PMAT00', '33', STATUS )

      END

      SUBROUTINE T_PMAT06 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P M A T 0 6
!  - - - - - - - - - - - - -
!
!  Test PMAT06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMAT06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBP(3,3)


      CALL PMAT06 ( 2400000.5D0, 50123.9999D0, RBP )

      CALL VVD ( RBP(1,1), 0.9999995505176007047D0, 1D-12,&
                 'PMAT06', '11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695404617348208406D-3, 1D-14,&
                 'PMAT06', '12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779735201865589104D-3, 1D-14,&
                 'PMAT06', '13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695404723772031414D-3, 1D-14,&
                 'PMAT06', '21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219496027161D0, 1D-12,&
                 'PMAT06', '22', STATUS )
      CALL VVD ( RBP(2,3), -0.1361752497080270143D-6, 1D-14,&
                 'PMAT06', '23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734957034089490D-3, 1D-14,&
                 'PMAT06', '31', STATUS )
      CALL VVD ( RBP(3,2), -0.1924880847894457113D-6, 1D-14,&
                 'PMAT06', '32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285679971958D0, 1D-12,&
                 'PMAT06', '33', STATUS )

      END

      SUBROUTINE T_PMAT76 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P M A T 7 6
!  - - - - - - - - - - - - -
!
!  Test PMAT76 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMAT76, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATP(3,3)


      CALL PMAT76 ( 2400000.5D0, 50123.9999D0, RMATP )

      CALL VVD ( RMATP(1,1), 0.9999995504328350733D0, 1D-12,&
                 'PMAT76', '11', STATUS )
      CALL VVD ( RMATP(1,2), 0.8696632209480960785D-3, 1D-14,&
                 'PMAT76', '12', STATUS )
      CALL VVD ( RMATP(1,3), 0.3779153474959888345D-3, 1D-14,&
                 'PMAT76', '13', STATUS )
      CALL VVD ( RMATP(2,1), -0.8696632209485112192D-3, 1D-14,&
                 'PMAT76', '21', STATUS )
      CALL VVD ( RMATP(2,2), 0.9999996218428560614D0, 1D-12,&
                 'PMAT76', '22', STATUS )
      CALL VVD ( RMATP(2,3), -0.1643284776111886407D-6, 1D-14,&
                 'PMAT76', '23', STATUS )
      CALL VVD ( RMATP(3,1), -0.3779153474950335077D-3, 1D-14,&
                 'PMAT76', '31', STATUS )
      CALL VVD ( RMATP(3,2), -0.1643306746147366896D-6, 1D-14,&
                 'PMAT76', '32', STATUS )
      CALL VVD ( RMATP(3,3), 0.9999999285899790119D0, 1D-12,&
                 'PMAT76', '33', STATUS )

      END

      SUBROUTINE T_PM ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ P M
!  - - - - - - - - -
!
!  Test PM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PM, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), R


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL PM ( P, R )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,&
                 'PM', ' ', STATUS )

      END

      SUBROUTINE T_PMP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P M P
!  - - - - - - - - - -
!
!  Test PMP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), AMB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL PMP ( A, B, AMB )

      CALL VVD ( AMB(1) + AMB(2) + AMB(3), -1D0, 1D-12,&
                 'PMP', ' ', STATUS )

      END

      SUBROUTINE T_PMPX ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P M P X
!  - - - - - - - - - - -
!
!  Test PMPX routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMPX, VVD
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC, DC, PR, PD, PX, RV, PMT, POB(3), PCO(3)


      RC = 1.234D0
      DC = 0.789D0
      PR = 1D-5
      PD = -2D-5
      PX = 1D-2
      RV = 10D0
      PMT = 8.75D0
      POB(1) = 0.9D0
      POB(2) = 0.4D0
      POB(3) = 0.1D0

      CALL PMPX ( RC, DC, PR, PD, PX, RV, PMT, POB, PCO )

      CALL VVD ( PCO(1), 0.2328137623960308438D0, 1D-12,&
                 'PMPX', '1', STATUS )
      CALL VVD ( PCO(2), 0.6651097085397855328D0, 1D-12,&
                 'PMPX', '2', STATUS )
      CALL VVD ( PCO(3), 0.7095257765896359837D0, 1D-12,&
                 'PMPX', '3', STATUS )

      END

      SUBROUTINE T_PMSAFE ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P M S A F E
!  - - - - - - - - - - - - -
!
!  Test PMSAFE routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PMSAFE, VVD, VIV
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION RA1, DEC1, PMR1, PMD1, PX1, RV1,&
                       EP1A, EP1B, EP2A, EP2B,&
                       RA2, DEC2, PMR2, PMD2, PX2, RV2


      RA1 = 1.234D0
      DEC1 = 0.789D0
      PMR1 = 1D-5
      PMD1 = -2D-5
      PX1 = 1D-2
      RV1 = 10D0
      EP1A = 2400000.5D0
      EP1B = 48348.5625D0
      EP2A = 2400000.5D0
      EP2B = 51544.5D0

      CALL PMSAFE ( RA1, DEC1, PMR1, PMD1, PX1, RV1,&
                        EP1A, EP1B, EP2A, EP2B,&
                        RA2, DEC2, PMR2, PMD2, PX2, RV2, J )

      CALL VVD ( RA2, 1.234087484501017061D0, 1D-12,&
                 'PMSAFE', 'RA2', STATUS )
      CALL VVD ( DEC2, 0.7888249982450468567D0, 1D-12,&
                 'PMSAFE', 'DEC2', STATUS )
      CALL VVD ( PMR2, 0.9996457663586073988D-5, 1D-12,&
                 'PMSAFE', 'PMR2', STATUS )
      CALL VVD ( PMD2, -0.2000040085106754565D-4, 1D-16,&
                 'PMSAFE', 'PMD2', STATUS )
      CALL VVD ( PX2, 0.9999997295356830666D-2, 1D-12,&
                 'PMSAFE', 'PX2', STATUS )
      CALL VVD ( RV2, 10.38468380293920069D0, 1D-10,&
                 'PMSAFE', 'RV2', STATUS )
      CALL VIV ( J, 0, 'PMSAFE', 'J', STATUS )

      END

      SUBROUTINE T_PN ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ P N
!  - - - - - - - - -
!
!  Test PN routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), R, U(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL PN ( P, R, U )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,&
                 'PN', 'R', STATUS )
      CALL VVD ( U(1), 0.1075552109073112058D0, 1D-12,&
                 'PN', 'U1', STATUS )
      CALL VVD ( U(2), 0.4302208436292448232D0, 1D-12,&
                 'PN', 'U2', STATUS )
      CALL VVD ( U(3), -0.8962934242275933816D0, 1D-12,&
                 'PN', 'U3', STATUS )

      END

      SUBROUTINE T_PN00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P N 0 0
!  - - - - - - - - - - -
!
!  Test PN00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,&
                       RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      DPSI = -0.9632552291149335877D-5
      DEPS = 0.4063197106621141414D-4

      CALL PN00 ( 2400000.5D0, 53736D0,&
                      DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,&
                 'PN00', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,&
                 'PN00', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-18,&
                 'PN00', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-18,&
                 'PN00', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-18,&
                 'PN00', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'PN00', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-18,&
                 'PN00', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-18,&
                 'PN00', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-18,&
                 'PN00', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'PN00', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,&
                 'PN00', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-18,&
                 'PN00', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-18,&
                 'PN00', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-18,&
                 'PN00', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,&
                 'PN00', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,&
                 'PN00', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,&
                 'PN00', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,&
                 'PN00', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,&
                 'PN00', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,&
                 'PN00', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,&
                 'PN00', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,&
                 'PN00', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,&
                 'PN00', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,&
                 'PN00', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,&
                 'PN00', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,&
                 'PN00', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,&
                 'PN00', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,&
                 'PN00', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,&
                 'PN00', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746144872140812D-5, 1D-16,&
                 'PN00', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831488838252590008D-5, 1D-16,&
                 'PN00', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837590456633197506D-5, 1D-16,&
                 'PN00', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692733D0, 1D-12,&
                 'PN00', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798559573702D-4, 1D-16,&
                 'PN00', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831847930135328368D-5, 1D-16,&
                 'PN00', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258150427D-4, 1D-16,&
                 'PN00', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806225D0, 1D-12,&
                 'PN00', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440499982806D0, 1D-12,&
                 'PN00', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332880253640848301D-2, 1D-14,&
                 'PN00', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760898731087295D-3, 1D-14,&
                 'PN00', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856746979948745D-2, 1D-14,&
                 'PN00', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109064768883D0, 1D-12,&
                 'PN00', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097740555723063806D-4, 1D-14,&
                 'PN00', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301929950205000D-3, 1D-14,&
                 'PN00', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020553681373702931D-4, 1D-14,&
                 'PN00', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958529887D0, 1D-12,&
                 'PN00', 'RBPN33', STATUS )

      END

      SUBROUTINE T_PN00A ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P N 0 0 A
!  - - - - - - - - - - - -
!
!  Test PN00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,&
                       RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL PN00A ( 2400000.5D0, 53736D0,&
                       DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9630909107115518431D-5, 1D-12,&
                 'PN00A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063239174001678710D-4, 1D-12,&
                 'PN00A', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,&
                 'PN00A', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,&
                 'PN00A', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,&
                 'PN00A', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,&
                 'PN00A', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,&
                 'PN00A', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'PN00A', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,&
                 'PN00A', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,&
                 'PN00A', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,&
                 'PN00A', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'PN00A', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,&
                 'PN00A', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-14,&
                 'PN00A', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-14,&
                 'PN00A', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-14,&
                 'PN00A', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,&
                 'PN00A', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,&
                 'PN00A', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,&
                 'PN00A', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,&
                 'PN00A', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,&
                 'PN00A', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,&
                 'PN00A', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,&
                 'PN00A', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,&
                 'PN00A', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,&
                 'PN00A', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,&
                 'PN00A', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,&
                 'PN00A', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,&
                 'PN00A', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,&
                 'PN00A', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,&
                 'PN00A', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536227949D0, 1D-12,&
                 'PN00A', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8836238544090873336D-5, 1D-14,&
                 'PN00A', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3830835237722400669D-5, 1D-14,&
                 'PN00A', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8836082880798569274D-5, 1D-14,&
                 'PN00A', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354655028D0, 1D-12,&
                 'PN00A', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063240865362499850D-4, 1D-14,&
                 'PN00A', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831194272065995866D-5, 1D-14,&
                 'PN00A', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063237480216291775D-4, 1D-14,&
                 'PN00A', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671660338D0, 1D-12,&
                 'PN00A', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440476103435D0, 1D-12,&
                 'PN00A', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332881761240011763D-2, 1D-14,&
                 'PN00A', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790767434730085751D-3, 1D-14,&
                 'PN00A', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332858254308954658D-2, 1D-14,&
                 'PN00A', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109044505577D0, 1D-12,&
                 'PN00A', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097782710396580452D-4, 1D-14,&
                 'PN00A', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791308472168152904D-3, 1D-14,&
                 'PN00A', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020595661591500259D-4, 1D-14,&
                 'PN00A', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314954572304D0, 1D-12,&
                 'PN00A', 'RBPN33', STATUS )

      END

      SUBROUTINE T_PN00B ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P N 0 0 B
!  - - - - - - - - - - - -
!
!  Test PN00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,&
                       RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL PN00B ( 2400000.5D0, 53736D0,&
                       DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9632552291148362783D-5, 1D-12,&
                 'PN00B', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063197106621159367D-4, 1D-12,&
                 'PN00B', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,&
                 'PN00B', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,&
                 'PN00B', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,&
                 'PN00B', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,&
                 'PN00B', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,&
                 'PN00B', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'PN00B', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,&
                 'PN00B', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,&
                 'PN00B', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,&
                 'PN00B', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'PN00B', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,&
                 'PN00B', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-14,&
                 'PN00B', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-14,&
                 'PN00B', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-14,&
                 'PN00B', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,&
                 'PN00B', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,&
                 'PN00B', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,&
                 'PN00B', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,&
                 'PN00B', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,&
                 'PN00B', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,&
                 'PN00B', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,&
                 'PN00B', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,&
                 'PN00B', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,&
                 'PN00B', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,&
                 'PN00B', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,&
                 'PN00B', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,&
                 'PN00B', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,&
                 'PN00B', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,&
                 'PN00B', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,&
                 'PN00B', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746144871248011D-5, 1D-14,&
                 'PN00B', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831488838252202945D-5, 1D-14,&
                 'PN00B', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837590456632304720D-5, 1D-14,&
                 'PN00B', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692733D0, 1D-12,&
                 'PN00B', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798559591654D-4, 1D-14,&
                 'PN00B', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831847930134941271D-5, 1D-14,&
                 'PN00B', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258168380D-4, 1D-14,&
                 'PN00B', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806225D0, 1D-12,&
                 'PN00B', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440499982806D0, 1D-12,&
                 'PN00B', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332880253640849194D-2, 1D-14,&
                 'PN00B', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760898731091166D-3, 1D-14,&
                 'PN00B', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856746979949638D-2, 1D-14,&
                 'PN00B', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109064768883D0, 1D-12,&
                 'PN00B', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097740555723081811D-4, 1D-14,&
                 'PN00B', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301929950208873D-3, 1D-14,&
                 'PN00B', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020553681373720832D-4, 1D-14,&
                 'PN00B', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958529887D0, 1D-12,&
                 'PN00B', 'RBPN33', STATUS )

      END

      SUBROUTINE T_PN06A ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P N 0 6 A
!  - - - - - - - - - - - -
!
!  Test PN06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,&
                       RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL PN06A ( 2400000.5D0, 53736D0,&
                       DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9630912025820308797D-5, 1D-12,&
                 'PN06A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063238496887249798D-4, 1D-14,&
                 'PN06A', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090789763356509926D0, 1D-14,&
                 'PN06A', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,&
                 'PN06A', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,&
                 'PN06A', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,&
                 'PN06A', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,&
                 'PN06A', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'PN06A', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,&
                 'PN06A', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,&
                 'PN06A', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,&
                 'PN06A', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'PN06A', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300536854831D0, 1D-12,&
                 'PN06A', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341646886204443795D-2, 1D-14,&
                 'PN06A', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880933488627759D-3, 1D-14,&
                 'PN06A', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341646890569782183D-2, 1D-14,&
                 'PN06A', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999913319321D0, 1D-12,&
                 'PN06A', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3835944216374477457D-6, 1D-14,&
                 'PN06A', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880833027867368D-3, 1D-14,&
                 'PN06A', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3985701514686976112D-6, 1D-14,&
                 'PN06A', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623534950D0, 1D-12,&
                 'PN06A', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300056797893D0, 1D-12,&
                 'PN06A', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717650545059598D-2, 1D-14,&
                 'PN06A', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075756493728856D-3, 1D-14,&
                 'PN06A', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341717674223918101D-2, 1D-14,&
                 'PN06A', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998963748448D0, 1D-12,&
                 'PN06A', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3504269280170069029D-6, 1D-14,&
                 'PN06A', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075211461454599D-3, 1D-14,&
                 'PN06A', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4316708436255949093D-6, 1D-14,&
                 'PN06A', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093032943D0, 1D-12,&
                 'PN06A', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536227668D0, 1D-12,&
                 'PN06A', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8836241998111535233D-5, 1D-14,&
                 'PN06A', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3830834608415287707D-5, 1D-14,&
                 'PN06A', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8836086334870740138D-5, 1D-14,&
                 'PN06A', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354657474D0, 1D-12,&
                 'PN06A', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063240188248455065D-4, 1D-14,&
                 'PN06A', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831193642839398128D-5, 1D-14,&
                 'PN06A', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063236803101479770D-4, 1D-14,&
                 'PN06A', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671663114D0, 1D-12,&
                 'PN06A', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440480669738D0, 1D-12,&
                 'PN06A', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332881418091915973D-2, 1D-14,&
                 'PN06A', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790767447612042565D-3, 1D-14,&
                 'PN06A', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332857911250989133D-2, 1D-14,&
                 'PN06A', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109049141908D0, 1D-12,&
                 'PN06A', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097767128546784878D-4, 1D-14,&
                 'PN06A', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791308482835292617D-3, 1D-14,&
                 'PN06A', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020580099454020310D-4, 1D-14,&
                 'PN06A', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314954628695D0, 1D-12,&
                 'PN06A', 'RBPN33', STATUS )

      END

      SUBROUTINE T_PN06 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P N 0 6
!  - - - - - - - - - - -
!
!  Test PN06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PN06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,&
                       RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      DPSI = -0.9632552291149335877D-5
      DEPS = 0.4063197106621141414D-4

      CALL PN06 ( 2400000.5D0, 53736D0,&
                      DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( EPSA, 0.4090789763356509926D0, 1D-12,&
                 'PN06', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,&
                 'PN06', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,&
                 'PN06', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,&
                 'PN06', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,&
                 'PN06', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,&
                 'PN06', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,&
                 'PN06', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,&
                 'PN06', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,&
                 'PN06', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,&
                 'PN06', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300536854831D0, 1D-12,&
                 'PN06', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341646886204443795D-2, 1D-14,&
                 'PN06', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880933488627759D-3, 1D-14,&
                 'PN06', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341646890569782183D-2, 1D-14,&
                 'PN06', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999913319321D0, 1D-12,&
                 'PN06', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3835944216374477457D-6, 1D-14,&
                 'PN06', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880833027867368D-3, 1D-14,&
                 'PN06', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3985701514686976112D-6, 1D-14,&
                 'PN06', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623534950D0, 1D-12,&
                 'PN06', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300056797893D0, 1D-12,&
                 'PN06', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717650545059598D-2, 1D-14,&
                 'PN06', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075756493728856D-3, 1D-14,&
                 'PN06', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341717674223918101D-2, 1D-14,&
                 'PN06', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998963748448D0, 1D-12,&
                 'PN06', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3504269280170069029D-6, 1D-14,&
                 'PN06', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075211461454599D-3, 1D-14,&
                 'PN06', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4316708436255949093D-6, 1D-14,&
                 'PN06', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093032943D0, 1D-12,&
                 'PN06', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,&
                 'PN06', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746921149881914D-5, 1D-14,&
                 'PN06', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831487047682968703D-5, 1D-14,&
                 'PN06', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837591232983692340D-5, 1D-14,&
                 'PN06', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692664D0, 1D-12,&
                 'PN06', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798558931215D-4, 1D-14,&
                 'PN06', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831846139597250235D-5, 1D-14,&
                 'PN06', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258792914D-4, 1D-14,&
                 'PN06', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806293D0, 1D-12,&
                 'PN06', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440504506688D0, 1D-12,&
                 'PN06', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332879913170492655D-2, 1D-14,&
                 'PN06', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760923225655753D-3, 1D-14,&
                 'PN06', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856406595754748D-2, 1D-14,&
                 'PN06', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109069366795D0, 1D-12,&
                 'PN06', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097725651142641812D-4, 1D-14,&
                 'PN06', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301952321296716D-3, 1D-14,&
                 'PN06', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020538796195230577D-4, 1D-14,&
                 'PN06', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958576778D0, 1D-12,&
                 'PN06', 'RBPN33', STATUS )

      END

      SUBROUTINE T_PNM00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P N M 0 0 A
!  - - - - - - - - - - - - -
!
!  Test PNM00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PNM00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL PNM00A ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832793134257D0, 1D-12,&
                 'PNM00A', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372384254137809439D-3, 1D-14,&
                 'PNM00A', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639684306407150645D-3, 1D-14,&
                 'PNM00A', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372535226570394543D-3, 1D-14,&
                 'PNM00A', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486491582471D0, 1D-12,&
                 'PNM00A', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132915262664072381D-4, 1D-14,&
                 'PNM00A', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639337004054317729D-3, 1D-14,&
                 'PNM00A', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163386925461775873D-4, 1D-14,&
                 'PNM00A', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329094390695D0, 1D-12,&
                 'PNM00A', '33', STATUS )

      END

      SUBROUTINE T_PNM00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P N M 0 0 B
!  - - - - - - - - - - - - -
!
!  Test PNM00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PNM00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL PNM00B ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832776208280D0, 1D-12,&
                 'PNM00B', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372401264429654837D-3, 1D-14,&
                 'PNM00B', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639691681450271771D-3, 1D-14,&
                 'PNM00B', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372552234147137424D-3, 1D-14,&
                 'PNM00B', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486477686123D0, 1D-12,&
                 'PNM00B', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132832190946052890D-4, 1D-14,&
                 'PNM00B', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639344385341866407D-3, 1D-14,&
                 'PNM00B', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163303977421522785D-4, 1D-14,&
                 'PNM00B', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329092049734D0, 1D-12,&
                 'PNM00B', '33', STATUS )

      END

      SUBROUTINE T_PNM06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P N M 0 6 A
!  - - - - - - - - - - - - -
!
!  Test PNM06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PNM06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL PNM06A ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832794205484D0, 1D-12,&
                 'PNM06A', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372382772630962111D-3, 1D-14,&
                 'PNM06A', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639684771140623099D-3, 1D-14,&
                 'PNM06A', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372533744743683605D-3, 1D-14,&
                 'PNM06A', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486492861646D0, 1D-12,&
                 'PNM06A', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132905944611019498D-4, 1D-14,&
                 'PNM06A', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639337469629464969D-3, 1D-14,&
                 'PNM06A', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163377605910663999D-4, 1D-14,&
                 'PNM06A', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329094260057D0, 1D-12,&
                 'PNM06A', '33', STATUS )

      END

      SUBROUTINE T_PNM80 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P N M 8 0
!  - - - - - - - - - - - -
!
!  Test PNM80 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PNM80, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATPN(3,3)


      CALL PNM80 ( 2400000.5D0, 50123.9999D0, RMATPN )

      CALL VVD ( RMATPN(1,1), 0.9999995831934611169D0, 1D-12,&
                 'PNM80', '11', STATUS )
      CALL VVD ( RMATPN(1,2), 0.8373654045728124011D-3, 1D-14,&
                 'PNM80', '12', STATUS )
      CALL VVD ( RMATPN(1,3), 0.3639121916933106191D-3, 1D-14,&
                 'PNM80', '13', STATUS )
      CALL VVD ( RMATPN(2,1), -0.8373804896118301316D-3, 1D-14,&
                 'PNM80', '21', STATUS )
      CALL VVD ( RMATPN(2,2), 0.9999996485439674092D0, 1D-12,&
                 'PNM80', '22', STATUS )
      CALL VVD ( RMATPN(2,3), 0.4130202510421549752D-4, 1D-14,&
                 'PNM80', '23', STATUS )
      CALL VVD ( RMATPN(3,1), -0.3638774789072144473D-3, 1D-14,&
                 'PNM80', '31', STATUS )
      CALL VVD ( RMATPN(3,2), -0.4160674085851722359D-4, 1D-14,&
                 'PNM80', '32', STATUS )
      CALL VVD ( RMATPN(3,3), 0.9999999329310274805D0, 1D-12,&
                 'PNM80', '33', STATUS )

      END

      SUBROUTINE T_POM00 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P O M 0 0
!  - - - - - - - - - - - -
!
!  Test POM00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  POM00, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XP, YP, SP, RPOM(3,3)


      XP = 2.55060238D-7
      YP = 1.860359247D-6
      SP = -0.1367174580728891460D-10

      CALL POM00 ( XP, YP, SP, RPOM )

      CALL VVD ( RPOM(1,1), 0.9999999999999674721D0, 1D-12,&
                 'POM00', '11', STATUS )
      CALL VVD ( RPOM(1,2), -0.1367174580728846989D-10, 1D-16,&
                 'POM00', '12', STATUS )
      CALL VVD ( RPOM(1,3), 0.2550602379999972345D-6, 1D-16,&
                 'POM00', '13', STATUS )
      CALL VVD ( RPOM(2,1), 0.1414624947957029801D-10, 1D-16,&
                 'POM00', '21', STATUS )
      CALL VVD ( RPOM(2,2), 0.9999999999982695317D0, 1D-12,&
                 'POM00', '22', STATUS )
      CALL VVD ( RPOM(2,3), -0.1860359246998866389D-5, 1D-16,&
                 'POM00', '23', STATUS )
      CALL VVD ( RPOM(3,1), -0.2550602379741215021D-6, 1D-16,&
                 'POM00', '31', STATUS )
      CALL VVD ( RPOM(3,2), 0.1860359247002414021D-5, 1D-16,&
                 'POM00', '32', STATUS )
      CALL VVD ( RPOM(3,3), 0.9999999999982370039D0, 1D-12,&
                 'POM00', '33', STATUS )

      END

      SUBROUTINE T_PPP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P P P
!  - - - - - - - - - -
!
!  Test PPP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PPP, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), APB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL PPP ( A, B, APB )

      CALL VVD ( APB(1) + APB(2) + APB(3), 15D0, 1D-12,&
                 'PPP', ' ', STATUS )

      END

      SUBROUTINE T_PPSP ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P P S P
!  - - - - - - - - - - -
!
!  Test PPSP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PPSP, VVD
!
!  This revision:  2008 November 30
!-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), S, B(3), APSB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      S = 5D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL PPSP ( A, S, B, APSB )

      CALL VVD ( APSB(1) + APSB(2) + APSB(3), 47D0, 1D-12,&
                 'PPSP', ' ', STATUS )

      END

      SUBROUTINE T_PR00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P R 0 0
!  - - - - - - - - - - -
!
!  Test PR00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PR00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSIPR, DEPSPR


      CALL PR00 ( 2400000.5D0, 53736D0, DPSIPR, DEPSPR )

      CALL VVD ( DPSIPR, -0.8716465172668347629D-7, 1D-22,&
                 'PR00', 'DPSIPR', STATUS )
      CALL VVD ( DEPSPR, -0.7342018386722813087D-8, 1D-22,&
                 'PR00', 'DEPSPR', STATUS )

      END

      SUBROUTINE T_PREC76 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P R E C 7 6
!  - - - - - - - - - - - - -
!
!  Test PREC76 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PREC76, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EP01, EP02, EP11, EP12, ZETA, Z, THETA


      EP01 = 2400000.5D0
      EP02 = 33282D0

      EP11 = 2400000.5D0
      EP12 = 51544D0

      CALL PREC76 ( EP01, EP02, EP11, EP12, ZETA, Z, THETA )

      CALL VVD ( ZETA, 0.5588961642000161243D-2, 1D-12,&
                 'PREC76', 'ZETA', STATUS )
      CALL VVD ( Z, 0.5589922365870680624D-2, 1D-12,&
                 'PREC76', 'Z', STATUS )
      CALL VVD ( THETA, 0.4858945471687296760D-2, 1D-12,&
                 'PREC76', 'THETA', STATUS )

      END

      SUBROUTINE T_PV2P ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P V 2 P
!  - - - - - - - - - - -
!
!  Test PV2P routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PV2P, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), P(3)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL PV2P ( PV, P )

      CALL VVD ( P(1), 0.3D0, 0D0, 'PV2P', '1', STATUS )
      CALL VVD ( P(2), 1.2D0, 0D0, 'PV2P', '2', STATUS )
      CALL VVD ( P(3), -2.5D0, 0D0, 'PV2P', '3', STATUS )

      END

      SUBROUTINE T_PV2S ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P V 2 S
!  - - - - - - - - - - -
!
!  Test PV2S routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PV2S, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), THETA, PHI, R, TD, PD, RD


      PV(1,1) = -0.4514964673880165D0
      PV(2,1) = 0.03093394277342585D0
      PV(3,1) = 0.05594668105108779D0

      PV(1,2) = 1.292270850663260D-5
      PV(2,2) = 2.652814182060692D-6
      PV(3,2) = 2.568431853930293D-6

      CALL PV2S ( PV, THETA, PHI, R, TD, PD, RD )

      CALL VVD ( THETA, 3.073185307179586515D0, 1D-12,&
                 'PV2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.1229999999999999992D0, 1D-12,&
                 'PV2S', 'PHI', STATUS )
      CALL VVD ( R, 0.4559999999999999757D0, 1D-12,&
                 'PV2S', 'R', STATUS )
      CALL VVD ( TD, -0.7800000000000000364D-5, 1D-16,&
                 'PV2S', 'TD', STATUS )
      CALL VVD ( PD, 0.9010000000000001639D-5, 1D-16,&
                 'PV2S', 'PD', STATUS )
      CALL VVD ( RD, -0.1229999999999999832D-4, 1D-16,&
                 'PV2S', 'RD', STATUS )

      END

      SUBROUTINE T_PVDPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P V D P V
!  - - - - - - - - - - - -
!
!  Test PVDPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVDPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), ADB(2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 6D0
      A(2,2) = 0D0
      A(3,2) = 4D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 0D0
      B(2,2) = 2D0
      B(3,2) = 8D0

      CALL PVDPV ( A, B, ADB )

      CALL VVD ( ADB(1), 20D0, 1D-12, 'PVDPV', '1', STATUS )
      CALL VVD ( ADB(2), 50D0, 1D-12, 'PVDPV', '2', STATUS )

      END

      SUBROUTINE T_PVM ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P V M
!  - - - - - - - - - -
!
!  Test PVM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVM, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), R, S


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.45D0
      PV(2,2) = -0.25D0
      PV(3,2) = 1.1D0

      CALL PVM ( PV, R, S )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,&
                 'PVM', 'R', STATUS )

      CALL VVD ( S, 1.214495780149111922D0, 1D-12,&
                 'PVM', 'S', STATUS )

      END

      SUBROUTINE T_PVMPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P V M P V
!  - - - - - - - - - - - -
!
!  Test PVMPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVMPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), AMB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 5D0
      A(2,2) = 6D0
      A(3,2) = 3D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 3D0
      B(2,2) = 2D0
      B(3,2) = 1D0

      CALL PVMPV ( A, B, AMB )

      CALL VVD ( AMB(1,1), 1D0, 1D-12, 'PVMPV', '11', STATUS )
      CALL VVD ( AMB(2,1), -1D0, 1D-12, 'PVMPV', '21', STATUS )
      CALL VVD ( AMB(3,1), -1D0, 1D-12, 'PVMPV', '31', STATUS )
      CALL VVD ( AMB(1,2), 2D0, 1D-12, 'PVMPV', '12', STATUS )
      CALL VVD ( AMB(2,2), 4D0, 1D-12, 'PVMPV', '22', STATUS )
      CALL VVD ( AMB(3,2), 2D0, 1D-12, 'PVMPV', '32', STATUS )

      END

      SUBROUTINE T_PVPPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P V P P V
!  - - - - - - - - - - - -
!
!  Test PVPPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVPPV, VVD
!
!  This revision:  2008 November 20
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), APB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 5D0
      A(2,2) = 6D0
      A(3,2) = 3D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 3D0
      B(2,2) = 2D0
      B(3,2) = 1D0

      CALL PVPPV ( A, B, APB )

      CALL VVD ( APB(1,1), 3D0, 1D-12, 'PVPPV', '11', STATUS )
      CALL VVD ( APB(2,1), 5D0, 1D-12, 'PVPPV', '21', STATUS )
      CALL VVD ( APB(3,1), 7D0, 1D-12, 'PVPPV', '31', STATUS )
      CALL VVD ( APB(1,2), 8D0, 1D-12, 'PVPPV', '12', STATUS )
      CALL VVD ( APB(2,2), 8D0, 1D-12, 'PVPPV', '22', STATUS )
      CALL VVD ( APB(3,2), 4D0, 1D-12, 'PVPPV', '32', STATUS )

      END

      SUBROUTINE T_PVSTAR ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ P V S T A R
!  - - - - - - - - - - - - -
!
!  Test PVSTAR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVSTAR, VVD, VIV
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  PV(3,2), RA, DEC, PMR, PMD, PX, RV
      INTEGER J


      PV(1,1) = 126668.5912743160601D0
      PV(2,1) = 2136.792716839935195D0
      PV(3,1) = -245251.2339876830091D0

      PV(1,2) = -0.4051854035740712739D-2
      PV(2,2) = -0.6253919754866173866D-2
      PV(3,2) = 0.1189353719774107189D-1

      CALL PVSTAR ( PV, RA, DEC, PMR, PMD, PX, RV, J )

      CALL VVD ( RA, 0.1686756D-1, 1D-12,&
                 'PVSTAR', 'RA', STATUS )
      CALL VVD ( DEC, -1.093989828D0, 1D-12,&
                 'PVSTAR', 'DEC', STATUS )
      CALL VVD ( PMR, -0.1783235160000472788D-4, 1D-16,&
                 'PVSTAR', 'PMR', STATUS )
      CALL VVD ( PMD, 0.2336024047000619347D-5, 1D-16,&
                 'PVSTAR', 'PMD', STATUS )
      CALL VVD ( PX, 0.74723D0, 1D-12,&
                 'PVSTAR', 'PX', STATUS )
      CALL VVD ( RV, -21.60000010107306010D0, 1D-11,&
                 'PVSTAR', 'RV', STATUS )
      CALL VIV ( J, 0, 'PVSTAR', 'J', STATUS )

      END

      SUBROUTINE T_PVTOB ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P V T O B
!  - - - - - - - - - - - -
!
!  Test PVTOB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVTOB, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION ELONG, PHI, HM, XP, YP, SP, THETA, PV(3,2)

      ELONG = 2D0
      PHI = 0.5D0
      HM = 3D3
      XP = 1D-6
      YP = -0.5D-6
      SP = 1D-8
      THETA = 5D0

      CALL PVTOB ( ELONG, PHI, HM, XP, YP, SP, THETA, PV )

      CALL VVD ( PV(1,1), 4225081.367071159207D0, 1D-5,&
                 'PVTOB', 'P(1)', STATUS )
      CALL VVD ( PV(2,1), 3681943.215856198144D0, 1D-5,&
                 'PVTOB', 'P(2)', STATUS )
      CALL VVD ( PV(3,1), 3041149.399241260785D0, 1D-5,&
                 'PVTOB', 'P(3)', STATUS )
      CALL VVD ( PV(1,2), -268.4915389365998787D0, 1D-9,&
                 'PVTOB', 'V(1)', STATUS )
      CALL VVD ( PV(2,2), 308.0977983288903123D0, 1D-9,&
                 'PVTOB', 'V(2)', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0,&
                 'PVTOB', 'V(3)', STATUS )

      END

      SUBROUTINE T_PVU( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P V U
!  - - - - - - - - - -
!
!  Test PVU routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVU, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), UPV(3,2)


      PV(1,1) = 126668.5912743160734D0
      PV(2,1) = 2136.792716839935565D0
      PV(3,1) = -245251.2339876830229D0

      PV(1,2) = -0.4051854035740713039D-2
      PV(2,2) = -0.6253919754866175788D-2
      PV(3,2) = 0.1189353719774107615D-1

      CALL PVU ( 2920D0, PV, UPV )

      CALL VVD ( UPV(1,1), 126656.7598605317105D0, 1D-12,&
                 'PVU', '11', STATUS )
      CALL VVD ( UPV(2,1), 2118.531271155726332D0, 1D-12,&
                 'PVU', '21', STATUS )
      CALL VVD ( UPV(3,1), -245216.5048590656190D0, 1D-12,&
                 'PVU', '31', STATUS )
      CALL VVD ( UPV(1,2), -0.4051854035740713039D-2, 1D-12,&
                 'PVU', '12', STATUS )
      CALL VVD ( UPV(2,2), -0.6253919754866175788D-2, 1D-12,&
                 'PVU', '22', STATUS )
      CALL VVD ( UPV(3,2), 0.1189353719774107615D-1, 1D-12,&
                 'PVU', '32', STATUS )

      END

      SUBROUTINE T_PVUP( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ P V U P
!  - - - - - - - - - - -
!
!  Test PVUP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVUP, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), P(3)


      PV(1,1) = 126668.5912743160734D0
      PV(2,1) = 2136.792716839935565D0
      PV(3,1) = -245251.2339876830229D0

      PV(1,2) = -0.4051854035740713039D-2
      PV(2,2) = -0.6253919754866175788D-2
      PV(3,2) = 0.1189353719774107615D-1

      CALL PVUP ( 2920D0, PV, P )

      CALL VVD ( P(1), 126656.7598605317105D0, 1D-12,&
                 'PVUP', '1', STATUS )
      CALL VVD ( P(2), 2118.531271155726332D0, 1D-12,&
                 'PVUP', '2', STATUS )
      CALL VVD ( P(3), -245216.5048590656190D0, 1D-12,&
                 'PVUP', '3', STATUS )

      END

      SUBROUTINE T_PVXPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ P V X P V
!  - - - - - - - - - - - -
!
!  Test PVXPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PVXPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), AXB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 6D0
      A(2,2) = 0D0
      A(3,2) = 4D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 0D0
      B(2,2) = 2D0
      B(3,2) = 8D0

      CALL PVXPV ( A, B, AXB )

      CALL VVD ( AXB(1,1), -1D0, 1D-12, 'PVXPV', '11', STATUS )
      CALL VVD ( AXB(2,1), -5D0, 1D-12, 'PVXPV', '21', STATUS )
      CALL VVD ( AXB(3,1), 4D0, 1D-12, 'PVXPV', '31', STATUS )
      CALL VVD ( AXB(1,2), -2D0, 1D-12, 'PVXPV', '12', STATUS )
      CALL VVD ( AXB(2,2), -36D0, 1D-12, 'PVXPV', '22', STATUS )
      CALL VVD ( AXB(3,2), 22D0, 1D-12, 'PVXPV', '32', STATUS )

      END

      SUBROUTINE T_PXP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ P X P
!  - - - - - - - - - -
!
!  Test PXP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  PXP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), AXB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL PXP ( A, B, AXB )

      CALL VVD ( AXB(1), -1D0, 1D-12, 'PXP', '1', STATUS )
      CALL VVD ( AXB(2), -5D0, 1D-12, 'PXP', '2', STATUS )
      CALL VVD ( AXB(3), 4D0, 1D-12, 'PXP', '3', STATUS )

      END

      SUBROUTINE T_REFCO ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ R E F C O
!  - - - - - - - - - - - -
!
!  Test REFCO routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  REFCO, VVD
!
!  This revision:  2013 September 24
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PHPA, TC, RH, WL, REFA, REFB


      PHPA = 800D0
      TC = 10D0
      RH = 0.9D0
      WL = 0.4D0

      CALL REFCO ( PHPA, TC, RH, WL, REFA, REFB )

      CALL VVD ( REFA, 0.2264949956241415009D-3, 1D-15,&
                 'REFCO', 'REFA', STATUS )
      CALL VVD ( REFB, -0.2598658261729343970D-6, 1D-18,&
                 'REFCO', 'REFB', STATUS )

      END

      SUBROUTINE T_RM2V ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ R M 2 V
!  - - - - - - - - - - -
!
!  Test RM2V routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RM2V, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), W(3)


      R(1,1) = 0D0
      R(1,2) = -0.80D0
      R(1,3) = -0.60D0

      R(2,1) = 0.80D0
      R(2,2) = -0.36D0
      R(2,3) = 0.48D0

      R(3,1) = 0.60D0
      R(3,2) = 0.48D0
      R(3,3) = -0.64D0

      CALL RM2V ( R, W )

      CALL VVD ( W(1), 0D0, 1D-12,&
                 'RM2V', '1', STATUS )
      CALL VVD ( W(2), 1.413716694115406957D0, 1D-12,&
                 'RM2V', '2', STATUS )
      CALL VVD ( W(3), -1.884955592153875943D0, 1D-12,&
                 'RM2V', '3', STATUS )

      END

      SUBROUTINE T_RV2M ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ R V 2 M
!  - - - - - - - - - - -
!
!  Test RV2M routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RV2M, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  W(3), R(3,3)


      W(1) = 0D0
      W(2) = 1.41371669D0
      W(3) = -1.88495559D0

      CALL RV2M ( W, R )

      CALL VVD ( R(1,1), -0.7071067782221119905D0, 1D-14,&
                 'RV2M', '11', STATUS )
      CALL VVD ( R(1,2), -0.5656854276809129651D0, 1D-14,&
                 'RV2M', '12', STATUS )
      CALL VVD ( R(1,3), -0.4242640700104211225D0, 1D-14,&
                 'RV2M', '13', STATUS )
      CALL VVD ( R(2,1), 0.5656854276809129651D0, 1D-14,&
                 'RV2M', '21', STATUS )
      CALL VVD ( R(2,2), -0.9254833945322742462D-1, 1D-14,&
                 'RV2M', '22', STATUS )
      CALL VVD ( R(2,3), -0.8194112531408833269D0, 1D-14,&
                 'RV2M', '23', STATUS )
      CALL VVD ( R(3,1), 0.4242640700104211225D0, 1D-14,&
                 'RV2M', '31', STATUS )
      CALL VVD ( R(3,2), -0.8194112531408833269D0, 1D-14,&
                 'RV2M', '32', STATUS )
      CALL VVD ( R(3,3), 0.3854415612311154341D0, 1D-14,&
                 'RV2M', '33', STATUS )

      END

      SUBROUTINE T_RX ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ R X
!  - - - - - - - - -
!
!  Test RX routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RX, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PHI, R(3,3)


      PHI = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL RX ( PHI, R )

      CALL VVD ( R(1,1), 2D0, 0D0,&
                 'RX', '11', STATUS )
      CALL VVD ( R(1,2), 3D0, 0D0,&
                 'RX', '12', STATUS )
      CALL VVD ( R(1,3), 2D0, 0D0,&
                 'RX', '13', STATUS )
      CALL VVD ( R(2,1), 3.839043388235612460D0, 1D-12,&
                 'RX', '21', STATUS )
      CALL VVD ( R(2,2), 3.237033249594111899D0, 1D-12,&
                 'RX', '22', STATUS )
      CALL VVD ( R(2,3), 4.516714379005982719D0, 1D-12,&
                 'RX', '23', STATUS )
      CALL VVD ( R(3,1), 1.806030415924501684D0, 1D-12,&
                 'RX', '31', STATUS )
      CALL VVD ( R(3,2), 3.085711545336372503D0, 1D-12,&
                 'RX', '32', STATUS )
      CALL VVD ( R(3,3), 3.687721683977873065D0, 1D-12,&
                 'RX', '33', STATUS )

      END

      SUBROUTINE T_RXP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ R X P
!  - - - - - - - - - -
!
!  Test RXP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RXP, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), P(3), RP(3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0
      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0
      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      P(1) = 0.2D0
      P(2) = 1.5D0
      P(3) = 0.1D0

      CALL RXP ( R, P, RP )

      CALL VVD ( RP(1), 5.1D0, 1D-12, 'RXP', '1', STATUS )
      CALL VVD ( RP(2), 3.9D0, 1D-12, 'RXP', '2', STATUS )
      CALL VVD ( RP(3), 7.10D0, 1D-12, 'RXP', '3', STATUS )

      END

      SUBROUTINE T_RXPV ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ R X P V
!  - - - - - - - - - - -
!
!  Test RXPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RXPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), PV(3,2), RPV(3,2)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      PV(1,1) = 0.2D0
      PV(2,1) = 1.5D0
      PV(3,1) = 0.1D0

      PV(1,2) = 1.5D0
      PV(2,2) = 0.2D0
      PV(3,2) = 0.1D0

      CALL RXPV ( R, PV, RPV )

      CALL VVD ( RPV(1,1), 5.1D0, 1D-12, 'RXPV', '11', STATUS )
      CALL VVD ( RPV(1,2), 3.8D0, 1D-12, 'RXPV', '12', STATUS )
      CALL VVD ( RPV(2,1), 3.9D0, 1D-12, 'RXPV', '21', STATUS )
      CALL VVD ( RPV(2,2), 5.2D0, 1D-12, 'RXPV', '22', STATUS )
      CALL VVD ( RPV(3,1), 7.1D0, 1D-12, 'RXPV', '31', STATUS )
      CALL VVD ( RPV(3,2), 5.8D0, 1D-12, 'RXPV', '32', STATUS )

      END

      SUBROUTINE T_RXR ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ R X R
!  - - - - - - - - - -
!
!  Test RXR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RXR, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,3), B(3,3), ATB(3,3)


      A(1,1) = 2D0
      A(1,2) = 3D0
      A(1,3) = 2D0

      A(2,1) = 3D0
      A(2,2) = 2D0
      A(2,3) = 3D0

      A(3,1) = 3D0
      A(3,2) = 4D0
      A(3,3) = 5D0

      B(1,1) = 1D0
      B(1,2) = 2D0
      B(1,3) = 2D0

      B(2,1) = 4D0
      B(2,2) = 1D0
      B(2,3) = 1D0

      B(3,1) = 3D0
      B(3,2) = 0D0
      B(3,3) = 1D0

      CALL RXR ( A, B, ATB )

      CALL VVD ( ATB(1,1), 20D0, 1D-12, 'RXR', '11', STATUS )
      CALL VVD ( ATB(1,2), 7D0, 1D-12, 'RXR', '12', STATUS )
      CALL VVD ( ATB(1,3), 9D0, 1D-12, 'RXR', '13', STATUS )
      CALL VVD ( ATB(2,1), 20D0, 1D-12, 'RXR', '21', STATUS )
      CALL VVD ( ATB(2,2), 8D0, 1D-12, 'RXR', '22', STATUS )
      CALL VVD ( ATB(2,3), 11D0, 1D-12, 'RXR', '23', STATUS )
      CALL VVD ( ATB(3,1), 34D0, 1D-12, 'RXR', '31', STATUS )
      CALL VVD ( ATB(3,2), 10D0, 1D-12, 'RXR', '32', STATUS )
      CALL VVD ( ATB(3,3), 15D0, 1D-12, 'RXR', '33', STATUS )

      END

      SUBROUTINE T_RY ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ R Y
!  - - - - - - - - -
!
!  Test RY routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RY, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION THETA, R(3,3)


      THETA = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL RY ( THETA, R )

      CALL VVD ( R(1,1), 0.8651847818978159930D0, 1D-12,&
                 'RY', '11', STATUS )
      CALL VVD ( R(1,2), 1.467194920539316554D0, 1D-12,&
                 'RY', '12', STATUS )
      CALL VVD ( R(1,3), 0.1875137911274457342D0, 1D-12,&
                 'RY', '13', STATUS )
      CALL VVD ( R(2,1), 3D0, 1D-12,&
                 'RY', '21', STATUS )
      CALL VVD ( R(2,2), 2D0, 1D-12,&
                 'RY', '22', STATUS )
      CALL VVD ( R(2,3), 3D0, 1D-12,&
                 'RY', '23', STATUS )
      CALL VVD ( R(3,1), 3.500207892850427330D0, 1D-12,&
                 'RY', '31', STATUS )
      CALL VVD ( R(3,2), 4.779889022262298150D0, 1D-12,&
                 'RY', '32', STATUS )
      CALL VVD ( R(3,3), 5.381899160903798712D0, 1D-12,&
                 'RY', '33', STATUS )

      END

      SUBROUTINE T_RZ ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ R Z
!  - - - - - - - - -
!
!  Test RZ routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  RZ, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PSI, R(3,3)


      PSI = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL RZ ( PSI, R )

      CALL VVD ( R(1,1), 2.898197754208926769D0, 1D-12,&
                 'RZ', '11', STATUS )
      CALL VVD ( R(1,2), 3.500207892850427330D0, 1D-12,&
                 'RZ', '12', STATUS )
      CALL VVD ( R(1,3), 2.898197754208926769D0, 1D-12,&
                 'RZ', '13', STATUS )
      CALL VVD ( R(2,1), 2.144865911309686813D0, 1D-12,&
                 'RZ', '21', STATUS )
      CALL VVD ( R(2,2), 0.8651847818978159930D0, 1D-12,&
                 'RZ', '22', STATUS )
      CALL VVD ( R(2,3), 2.144865911309686813D0, 1D-12,&
                 'RZ', '23', STATUS )
      CALL VVD ( R(3,1), 3D0, 1D-12,&
                 'RZ', '31', STATUS )
      CALL VVD ( R(3,2), 4D0, 1D-12,&
                 'RZ', '32', STATUS )
      CALL VVD ( R(3,3), 5D0, 1D-12,&
                 'RZ', '33', STATUS )

      END

      SUBROUTINE T_S00A ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S 0 0 A
!  - - - - - - - - - - -
!
!  Test S00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( S00A ( 2400000.5D0, 52541D0 ),&
                 -0.1340684448919163584D-7, 1D-18,&
                 'S00A', ' ', STATUS )

      END

      SUBROUTINE T_S00B ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S 0 0 B
!  - - - - - - - - - - -
!
!  Test S00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( S00B ( 2400000.5D0, 52541D0 ),&
                 -0.1340695782951026584D-7, 1D-18,&
                 'S00B', ' ', STATUS )

      END

      SUBROUTINE T_S00 ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ S 0 0
!  - - - - - - - - - -
!
!  Test S00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL VVD ( S00 ( 2400000.5D0, 53736D0, X, Y ),&
                 -0.1220036263270905693D-7, 1D-18,&
                 'S00', ' ', STATUS )

      END

      SUBROUTINE T_S06A ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S 0 6 A
!  - - - - - - - - - - -
!
!  Test S06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( S06A ( 2400000.5D0, 52541D0 ),&
                 -0.1340680437291812383D-7, 1D-18,&
                 'S06A', ' ', STATUS )

      END

      SUBROUTINE T_S06 ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ S 0 6
!  - - - - - - - - - -
!
!  Test S06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL VVD ( S06 ( 2400000.5D0, 53736D0, X, Y ),&
                 -0.1220032213076463117D-7, 1D-18,&
                 'S06', ' ', STATUS )

      END

      SUBROUTINE T_S2C ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ S 2 C
!  - - - - - - - - - -
!
!  Test S2C routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S2C, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION C(3)


      CALL S2C ( 3.0123D0, -0.999D0, C )

      CALL VVD ( C(1), -0.5366267667260523906D0, 1D-12,&
                 'S2C', '1', STATUS )
      CALL VVD ( C(2), 0.6977111097651453650D-1, 1D-12,&
                 'S2C', '2', STATUS )
      CALL VVD ( C(3), -0.8409302618566214041D0, 1D-12,&
                 'S2C', '3', STATUS )

      END

      SUBROUTINE T_S2P ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ S 2 P
!  - - - - - - - - - -
!
!  Test S2P routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S2P, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3)


      CALL S2P ( -3.21D0, 0.123D0, 0.456D0, P )

      CALL VVD ( P(1), -0.4514964673880165228D0, 1D-12,&
                 'S2P', 'X', STATUS )
      CALL VVD ( P(2),  0.3093394277342586880D-1, 1D-12,&
                 'S2P', 'Y', STATUS )
      CALL VVD ( P(3),  0.5594668105108779333D-1, 1D-12,&
                 'S2P', 'Z', STATUS )

      END

      SUBROUTINE T_S2PV ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S 2 P V
!  - - - - - - - - - - -
!
!  Test S2PV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S2PV, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)


      CALL S2PV ( -3.21D0, 0.123D0, 0.456D0, -7.8D-6, 9.01D-6,&
                      -1.23D-5, PV )

      CALL VVD ( PV(1,1), -0.4514964673880165228D0, 1D-12,&
                 'S2PV', 'X', STATUS )
      CALL VVD ( PV(2,1),  0.3093394277342586880D-1, 1D-12,&
                 'S2PV', 'Y', STATUS )
      CALL VVD ( PV(3,1),  0.5594668105108779333D-1, 1D-12,&
                 'S2PV', 'Z', STATUS )
      CALL VVD ( PV(1,2),  0.1292270850663260170D-4, 1D-16,&
                 'S2PV', 'VX', STATUS )
      CALL VVD ( PV(2,2),  0.2652814182060691422D-5, 1D-16,&
                 'S2PV', 'VY', STATUS )
      CALL VVD ( PV(3,2),  0.2568431853930292259D-5, 1D-16,&
                 'S2PV', 'VZ', STATUS )

      END

      SUBROUTINE T_S2XPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ S 2 X P V
!  - - - - - - - - - - - -
!
!  Test S2XPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  S2XPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S1, S2, PV(3,2), SPV(3,2)


      S1 = 2D0
      S2 = 3D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.5D0
      PV(2,2) = 2.3D0
      PV(3,2) = -0.4D0

      CALL S2XPV ( S1, S2, PV, SPV )

      CALL VVD ( SPV(1,1), 0.6D0, 1D-12, 'S2XPV', 'P1', STATUS )
      CALL VVD ( SPV(2,1), 2.4D0, 1D-12, 'S2XPV', 'P2', STATUS )
      CALL VVD ( SPV(3,1), -5.0D0, 1D-12, 'S2XPV', 'P3', STATUS )
      CALL VVD ( SPV(1,2), 1.5D0, 1D-12, 'S2XPV', 'V1', STATUS )
      CALL VVD ( SPV(2,2), 6.9D0, 1D-12, 'S2XPV', 'V2', STATUS )
      CALL VVD ( SPV(3,2), -1.2D0, 1D-12, 'S2XPV', 'V3', STATUS )

      END

      SUBROUTINE T_SEPP ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S E P P
!  - - - - - - - - - - -
!
!  Test SEPP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  SEPP, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), S


      A(1) = 1D0
      A(2) = 0.1D0
      A(3) = 0.2D0

      B(1) = -3D0
      B(2) = 1D-3
      B(3) = 0.2D0

      CALL SEPP ( A, B, S )

      CALL VVD ( S, 2.860391919024660768D0, 1D-12,&
                 'SEPP', ' ', STATUS )

      END

      SUBROUTINE T_SEPS ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S E P S
!  - - - - - - - - - - -
!
!  Test SEPS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  SEPS, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION AL, AP, BL, BP, S


      AL = 1D0
      AP = 0.1D0

      BL = 0.2D0
      BP = -3D0

      CALL SEPS ( AL, AP, BL, BP, S )

      CALL VVD ( S, 2.346722016996998842D0, 1D-14,&
                 'SEPS', ' ', STATUS )

      END

      SUBROUTINE T_SP00 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S P 0 0
!  - - - - - - - - - - -
!
!  Test SP00 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  SP00, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      CALL VVD ( SP00 (2400000.5D0, 52541D0),&
                 -0.6216698469981019309D-11, 1D-12,&
                 'SP00', ' ', STATUS )

      END

      SUBROUTINE T_STARPM ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ S T A R P M
!  - - - - - - - - - - - - -
!
!  Test STARPM routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  STARPM, VVD, VIV
!
!  This revision:  2017 March 15
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA1, DEC1, PMR1, PMD1, PX1, RV1,&
                       RA2, DEC2, PMR2, PMD2, PX2, RV2
      INTEGER J


      RA1 = 0.01686756D0
      DEC1 = -1.093989828D0
      PMR1 = -1.78323516D-5
      PMD1 = 2.336024047D-6
      PX1 = 0.74723D0
      RV1 = -21.6D0

      CALL STARPM ( RA1, DEC1, PMR1, PMD1, PX1, RV1,&
                        2400000.5D0, 50083D0, 2400000.5D0, 53736D0,&
                        RA2, DEC2, PMR2, PMD2, PX2, RV2, J )

      CALL VVD ( RA2, 0.1668919069414256149D-1, 1D-13,&
                 'STARPM', 'RA', STATUS )
      CALL VVD ( DEC2, -1.093966454217127897D0, 1D-13,&
                 'STARPM', 'DEC', STATUS )
      CALL VVD ( PMR2, -0.1783662682153176524D-4, 1D-17,&
                 'STARPM', 'PMR', STATUS )
      CALL VVD ( PMD2, 0.2338092915983989595D-5, 1D-17,&
                 'STARPM', 'PMD', STATUS )
      CALL VVD ( PX2, 0.7473533835317719243D0, 1D-13,&
                 'STARPM', 'PX', STATUS )
      CALL VVD ( RV2, -21.59905170476417175D0, 1D-11,&
                 'STARPM', 'RV', STATUS )
      CALL VIV ( J, 0, 'STARPM', 'J', STATUS )

      END

      SUBROUTINE T_STARPV ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ S T A R P V
!  - - - - - - - - - - - - -
!
!  Test STARPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  STARPV, VVD, VIV
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA, DEC, PMR, PMD, PX, RV, PV(3,2)
      INTEGER J


      RA = 0.01686756D0
      DEC = -1.093989828D0
      PMR = -1.78323516D-5
      PMD = 2.336024047D-6
      PX = 0.74723D0
      RV = -21.6D0

      CALL STARPV ( RA, DEC, PMR, PMD, PX, RV, PV, J )

      CALL VVD ( PV(1,1), 126668.5912743160601D0, 1D-10,&
                 'STARPV', '11', STATUS )
      CALL VVD ( PV(2,1), 2136.792716839935195D0, 1D-12,&
                 'STARPV', '21', STATUS )
      CALL VVD ( PV(3,1), -245251.2339876830091D0, 1D-10,&
                 'STARPV', '31', STATUS )
      CALL VVD ( PV(1,2), -0.4051854008955659551D-2, 1D-13,&
                 'STARPV', '12', STATUS )
      CALL VVD ( PV(2,2), -0.6253919754414777970D-2, 1D-15,&
                 'STARPV', '22', STATUS )
      CALL VVD ( PV(3,2), 0.1189353714588109341D-1, 1D-13,&
                 'STARPV', '32', STATUS )
      CALL VIV ( J, 0, 'STARPV', 'J', STATUS )

      END

      SUBROUTINE T_SXP ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ S X P
!  - - - - - - - - - -
!
!  Test SXP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  SXP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S, P(3), SP(3)


      S = 2D0

      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL SXP ( S, P, SP )

      CALL VVD ( SP(1), 0.6D0, 0D0, 'SXP', '1', STATUS )
      CALL VVD ( SP(2), 2.4D0, 0D0, 'SXP', '2', STATUS )
      CALL VVD ( SP(3), -5D0, 0D0, 'SXP', '3', STATUS )

      END


      SUBROUTINE T_SXPV ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ S X P V
!  - - - - - - - - - - -
!
!  Test SXPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  SXPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S, PV(3,2), SPV(3,2)


      S = 2D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.5D0
      PV(2,2) = 3.2D0
      PV(3,2) = -0.7D0

      CALL SXPV ( S, PV, SPV )

      CALL VVD ( SPV(1,1), 0.6D0, 0D0, 'SXPV', 'P1', STATUS )
      CALL VVD ( SPV(2,1), 2.4D0, 0D0, 'SXPV', 'P2', STATUS )
      CALL VVD ( SPV(3,1), -5D0, 0D0, 'SXPV', 'P3', STATUS )
      CALL VVD ( SPV(1,2), 1D0, 0D0, 'SXPV', 'V1', STATUS )
      CALL VVD ( SPV(2,2), 6.4D0, 0D0, 'SXPV', 'V2', STATUS )
      CALL VVD ( SPV(3,2), -1.4D0, 0D0, 'SXPV', 'V3', STATUS )

      END

      SUBROUTINE T_TAITT ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T A I T T
!  - - - - - - - - - - - -
!
!  Test TAITT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TAITT, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL TAITT ( 2453750.5D0, 0.892482639D0, T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'TAITT', 'T1', STATUS )
      CALL VVD ( T2, 0.892855139D0, 1D-12, 'TAITT', 'T2', STATUS )
      CALL VIV ( J, 0, 'TAITT', 'J', STATUS )

      END

      SUBROUTINE T_TAIUT1 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ T A I U T 1
!  - - - - - - - - - - - - -
!
!  Test TAIUT1 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TAIUT1, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL TAIUT1 ( 2453750.5D0, 0.892482639D0, -32.6659D0,&
                        U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'TAIUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045614537037037D0, 1D-12,&
                 'TAIUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'TAIUT1', 'J', STATUS )

      END

      SUBROUTINE T_TAIUTC ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ T A I U T C
!  - - - - - - - - - - - - -
!
!  Test TAIUTC routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TAIUTC, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL TAIUTC ( 2453750.5D0, 0.892482639D0, U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'TAIUTC', 'U1', STATUS )
      CALL VVD ( U2, 0.8921006945555555556D0, 1D-12,&
                 'TAIUTC', 'U2', STATUS )
      CALL VIV ( J, 0, 'TAIUTC', 'J', STATUS )

      END

      SUBROUTINE T_TCBTDB ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ T C B T D B
!  - - - - - - - - - - - - -
!
!  Test TCBTDB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TCBTDB, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL TCBTDB ( 2453750.5D0, 0.893019599D0, B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'TCBTDB', 'B1', STATUS )
      CALL VVD ( B2, 0.8928551362746343397D0, 1D-12,&
                 'TCBTDB', 'B2', STATUS )
      CALL VIV ( J, 0, 'TCBTDB', 'J', STATUS )

      END

      SUBROUTINE T_TCGTT ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T C G T T
!  - - - - - - - - - - - -
!
!  Test TCGTT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TCGTT, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL TCGTT ( 2453750.5D0, 0.892862531D0, T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'TCGTT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551387488816828D0, 1D-12,&
                 'TCGTT', 'T2', STATUS )
      CALL VIV ( J, 0, 'TCGTT', 'J', STATUS )

      END

      SUBROUTINE T_TDBTCB ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ T D B T C B
!  - - - - - - - - - - - - -
!
!  Test TDBTCB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TDBTCB, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL TDBTCB ( 2453750.5D0, 0.892855137D0, B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'TDBTCB', 'B1', STATUS )
      CALL VVD ( B2, 0.8930195997253656716D0, 1D-12,&
                 'TDBTCB', 'B2', STATUS )
      CALL VIV ( J, 0, 'TDBTCB', 'J', STATUS )

      END

      SUBROUTINE T_TDBTT ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T D B T T
!  - - - - - - - - - - - -
!
!  Test TDBTT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TDBTT, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL TDBTT ( 2453750.5D0, 0.892855137D0, -0.000201D0,&
                       T1, T2, J )

      CALL VVD ( T1,  2453750.5D0, 1D-6, 'TDBTT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551393263888889D0, 1D-12,&
                 'TDBTT', 'T2', STATUS )
      CALL VIV ( J, 0, 'TDBTT', 'J', STATUS )

      END

      SUBROUTINE T_TF2A ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ T F 2 A
!  - - - - - - - - - - -
!
!  Test TF2A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TF2A, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A
      INTEGER J


      CALL TF2A ( '+', 4, 58, 20.2D0, A, J )

      CALL VVD ( A, 1.301739278189537429D0, 1D-12,&
                 'TF2A', 'A', STATUS )
      CALL VIV ( J, 0, 'TF2A', 'J', STATUS )

      END

      SUBROUTINE T_TF2D ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ T F 2 D
!  - - - - - - - - - - -
!
!  Test TF2D routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TF2D, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION D
      INTEGER J


      CALL TF2D ( ' ', 23, 55, 10.9D0, D, J )

      CALL VVD ( D, 0.9966539351851851852D0, 1D-12,&
                 'TF2D', 'D', STATUS )
      CALL VIV ( J, 0, 'TF2D', 'J', STATUS )

      END

      SUBROUTINE T_TPORS ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P O R S
!  - - - - - - - - - - - -
!
!  Test TPORS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPORS, VIV, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XI, ETA, RA, DEC, AZ1, BZ1, AZ2, BZ2
      INTEGER N


      XI = -0.03D0
      ETA = 0.07D0
      RA = 1.3D0
      DEC = 1.5D0

      CALL TPORS ( XI, ETA, RA, DEC, AZ1, BZ1, AZ2, BZ2, N )

      CALL VVD ( AZ1, 1.736621577783208748D0, 1D-13,&
                 'TPORS', 'AZ1', STATUS )
      CALL VVD ( BZ1, 1.436736561844090323D0, 1D-13,&
                 'TPORS', 'BZ1', STATUS )

      CALL VVD ( AZ2, 4.004971075806584490D0, 1D-13,&
                 'TPORS', 'AZ2', STATUS )
      CALL VVD ( BZ2, 1.565084088476417917D0, 1D-13,&
                 'TPORS', 'BZ2', STATUS )

      CALL VIV ( N, 2, 'TPORS', 'N', STATUS )

      END

      SUBROUTINE T_TPORV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P O R V
!  - - - - - - - - - - - -
!
!  Test TPORV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPORV, S2C, VVD, VIV
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XI, ETA, RA, DEC, V(3), VZ1(3), VZ2(3)
      INTEGER N


      XI = -0.03D0
      ETA = 0.07D0
      RA = 1.3D0
      DEC = 1.5D0
      CALL S2C ( RA, DEC, V )

      CALL TPORV ( XI, ETA, V, VZ1, VZ2, N )

      CALL VVD ( VZ1(1), -0.02206252822366888610D0, 1D-15,&
                 'TPORV', 'X1', STATUS )
      CALL VVD ( VZ1(2), 0.1318251060359645016D0, 1D-14,&
                 'TPORV', 'Y1', STATUS )
      CALL VVD ( VZ1(3), 0.9910274397144543895D0, 1D-14,&
                 'TPORV', 'Z1', STATUS )

      CALL VVD ( VZ2(1), -0.003712211763801968173D0, 1D-16,&
                 'TPORV', 'X2', STATUS )
      CALL VVD ( VZ2(2), -0.004341519956299836813D0, 1D-16,&
                 'TPORV', 'Y2', STATUS )
      CALL VVD ( VZ2(3), 0.9999836852110587012D0, 1D-14,&
                 'TPORV', 'Z2', STATUS )

      CALL VIV ( N, 2, 'TPORV', 'N', STATUS )

      END

      SUBROUTINE T_TPSTS ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P S T S
!  - - - - - - - - - - - -
!
!  Test TPSTS routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPSTS, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XI, ETA, RAZ, DECZ, RA, DEC


      XI = -0.03D0
      ETA = 0.07D0
      RAZ = 2.3D0
      DECZ = 1.5D0

      CALL TPSTS ( XI, ETA, RAZ, DECZ, RA, DEC )

      CALL VVD ( RA, 0.7596127167359629775D0, 1D-14,&
                 'TPSTS', 'RA', STATUS )
      CALL VVD ( DEC, 1.540864645109263028D0, 1D-13,&
                 'TPSTS', 'DEC', STATUS )

      END

      SUBROUTINE T_TPSTV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P S T V
!  - - - - - - - - - - - -
!
!  Test TPSTV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPSTV, S2C, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XI, ETA, RAZ, DECZ, VZ(3), V(3)


      XI = -0.03D0
      ETA = 0.07D0
      RAZ = 2.3D0
      DECZ = 1.5D0
      CALL S2C ( RAZ, DECZ, VZ )

      CALL TPSTV( XI, ETA, VZ, V )

      CALL VVD ( V(1), 0.02170030454907376677D0, 1D-15,&
                 'TPSTV', 'X', STATUS )
      CALL VVD ( V(2), 0.02060909590535367447D0, 1D-15,&
                 'TPSTV', 'Y', STATUS )
      CALL VVD ( V(3), 0.9995520806583523804D0, 1D-14,&
                 'TPSTV', 'Z', STATUS )

      END

      SUBROUTINE T_TPXES ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P X E S
!  - - - - - - - - - - - -
!
!  Test TPXES routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPXES, VVD, VIV
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA, DEC, RAZ, DECZ, XI, ETA
      INTEGER J


      RA = 1.3D0
      DEC = 1.55D0
      RAZ = 2.3D0
      DECZ = 1.5D0

      CALL TPXES ( RA, DEC, RAZ, DECZ, XI, ETA, J )

      CALL VVD ( XI, -0.01753200983236980595D0, 1D-15,&
                 'TPXES', 'XI', STATUS )
      CALL VVD ( ETA, 0.05962940005778712891D0, 1D-15,&
                 'TPXES', 'ETA', STATUS )
      CALL VIV ( J, 0, 'TPXES', 'J', STATUS )

      END

      SUBROUTINE T_TPXEV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T P X E V
!  - - - - - - - - - - - -
!
!  Test TPXEV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TPXEV, S2C, VVD
!
!  This revision:  2017 October 21
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA, DEC, RAZ, DECZ, V(3), VZ(3), XI, ETA
      INTEGER J


      RA = 1.3D0
      DEC = 1.55D0
      RAZ = 2.3D0
      DECZ = 1.5D0
      CALL S2C ( RA, DEC, V )
      CALL S2C ( RAZ, DECZ, VZ )

      CALL TPXEV( V, VZ, XI, ETA, J  )

      CALL VVD ( XI, -0.01753200983236980595D0, 1D-15,&
                 'TPXEV', 'XI', STATUS )
      CALL VVD ( ETA, 0.05962940005778712891D0, 1D-15,&
                 'TPXEV', 'ETA', STATUS )
      CALL VIV ( J, 0, 'TPXEV', 'J', STATUS )

      END

      SUBROUTINE T_TR ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ T R
!  - - - - - - - - -
!
!  Test TR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TR, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), RT(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL TR ( R, RT )

      CALL VVD ( RT(1,1), 2D0, 0D0, 'TR', '11', STATUS )
      CALL VVD ( RT(1,2), 3D0, 0D0, 'TR', '12', STATUS )
      CALL VVD ( RT(1,3), 3D0, 0D0, 'TR', '13', STATUS )
      CALL VVD ( RT(2,1), 3D0, 0D0, 'TR', '21', STATUS )
      CALL VVD ( RT(2,2), 2D0, 0D0, 'TR', '22', STATUS )
      CALL VVD ( RT(2,3), 4D0, 0D0, 'TR', '23', STATUS )
      CALL VVD ( RT(3,1), 2D0, 0D0, 'TR', '31', STATUS )
      CALL VVD ( RT(3,2), 3D0, 0D0, 'TR', '32', STATUS )
      CALL VVD ( RT(3,3), 5D0, 0D0, 'TR', '33', STATUS )

      END

      SUBROUTINE T_TRXP ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ T R X P
!  - - - - - - - - - - -
!
!  Test TRXP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TRXP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), P(3), TRP(3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      P(1) = 0.2D0
      P(2) = 1.5D0
      P(3) = 0.1D0

      CALL TRXP ( R, P, TRP )

      CALL VVD ( TRP(1), 5.2D0, 1D-12, 'TRXP', '1', STATUS )
      CALL VVD ( TRP(2), 4D0, 1D-12, 'TRXP', '2', STATUS )
      CALL VVD ( TRP(3), 5.4D0, 1D-12, 'TRXP', '3', STATUS )

      END

      SUBROUTINE T_TRXPV ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T R X P V
!  - - - - - - - - - - - -
!
!  Test TRXPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TRXPV, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), PV(3,2), TRPV(3,2)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      PV(1,1) = 0.2D0
      PV(2,1) = 1.5D0
      PV(3,1) = 0.1D0

      PV(1,2) = 1.5D0
      PV(2,2) = 0.2D0
      PV(3,2) = 0.1D0

      CALL TRXPV ( R, PV, TRPV )

      CALL VVD ( TRPV(1,1), 5.2D0, 1D-12, 'TRXPV', 'P1', STATUS )
      CALL VVD ( TRPV(2,1), 4D0,   1D-12, 'TRXPV', 'P2', STATUS )
      CALL VVD ( TRPV(3,1), 5.4D0, 1D-12, 'TRXPV', 'P3', STATUS )
      CALL VVD ( TRPV(1,2), 3.9D0, 1D-12, 'TRXPV', 'V1', STATUS )
      CALL VVD ( TRPV(2,2), 5.3D0, 1D-12, 'TRXPV', 'V2', STATUS )
      CALL VVD ( TRPV(3,2), 4.1D0, 1D-12, 'TRXPV', 'V3', STATUS )

      END

      SUBROUTINE T_TTTAI ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T T T A I
!  - - - - - - - - - - - -
!
!  Test TTTAI routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TTTAI, VVD, VIV
!
!  This revision:  2010 September 7
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A1, A2
      INTEGER J


      CALL TTTAI ( 2453750.5D0, 0.892482639D0, A1, A2, J )

      CALL VVD ( A1, 2453750.5D0, 1D-6, 'TTTAI', 'A1', STATUS )
      CALL VVD ( A2, 0.892110139D0, 1D-12,&
                 'TTTAI', 'A2', STATUS )
      CALL VIV ( J, 0, 'TTTAI', 'J', STATUS )

      END

      SUBROUTINE T_TTTCG ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T T T C G
!  - - - - - - - - - - - -
!
!  Test TTTCG routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TTTCG, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION G1, G2
      INTEGER J


      CALL TTTCG ( 2453750.5D0, 0.892482639D0, G1, G2, J )

      CALL VVD ( G1, 2453750.5D0, 1D-6, 'TTTCG', 'G1', STATUS )
      CALL VVD ( G2, 0.8924900312508587113D0, 1D-12,&
                 'TTTCG', 'G2', STATUS )
      CALL VIV ( J, 0, 'TTTCG', 'J', STATUS )

      END

      SUBROUTINE T_TTTDB ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T T T D B
!  - - - - - - - - - - - -
!
!  Test TTTDB routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TTTDB, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL TTTDB ( 2453750.5D0, 0.892855139D0, -0.000201D0,&
                       B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'TTTDB', 'B1', STATUS )
      CALL VVD ( B2, 0.8928551366736111111D0, 1D-12,&
                 'TTTDB', 'B2', STATUS )
      CALL VIV ( J, 0, 'TTTDB', 'J', STATUS )

      END

      SUBROUTINE T_TTUT1 ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ T T U T 1
!  - - - - - - - - - - - -
!
!  Test TTUT1 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  TTUT1, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL TTUT1 ( 2453750.5D0, 0.892855139D0, 64.8499D0,&
                       U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'TTUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045614537037037D0, 1D-12,&
                 'TTUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'TTUT1', 'J', STATUS )

      END

      SUBROUTINE T_UT1TAI ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ U T 1 T A I
!  - - - - - - - - - - - - -
!
!  Test UT1TAI routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  UT1TAI, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A1, A2
      INTEGER J


      CALL UT1TAI ( 2453750.5D0, 0.892104561D0, -32.6659D0,&
                        A1, A2, J )

      CALL VVD ( A1, 2453750.5D0, 1D-6, 'UT1TAI', 'A1', STATUS )
      CALL VVD ( A2, 0.8924826385462962963D0, 1D-12,&
                 'UT1TAI', 'A2', STATUS )
      CALL VIV ( J, 0, 'UT1TAI', 'J', STATUS )

      END

      SUBROUTINE T_UT1TT ( STATUS )
!+
!  - - - - - - - - - - - -
!   T _ i a u _ U T 1 T T
!  - - - - - - - - - - - -
!
!  Test UT1TT routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  UT1TT, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL UT1TT ( 2453750.5D0, 0.892104561D0, 64.8499D0,&
                       T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'UT1TT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551385462962963D0, 1D-12,&
                 'UT1TT', 'T2', STATUS )
      CALL VIV ( J, 0, 'UT1TT', 'J', STATUS )

      END

      SUBROUTINE T_UT1UTC ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ U T 1 U T C
!  - - - - - - - - - - - - -
!
!  Test UT1UTC routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  UT1UTC, VVD, VIV
!
!  This revision:  2010 September 6
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL UT1UTC ( 2453750.5D0, 0.892104561D0, 0.3341D0,&
                        U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'UT1UTC', 'U1', STATUS )
      CALL VVD ( U2, 0.8921006941018518519D0, 1D-12,&
                 'UT1UTC', 'U2', STATUS )
      CALL VIV ( J, 0, 'UT1UTC', 'J', STATUS )

      END

      SUBROUTINE T_UTCTAI ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ U T C T A I
!  - - - - - - - - - - - - -
!
!  Test UTCTAI routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  UTCTAI, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL UTCTAI ( 2453750.5D0, 0.892100694D0, U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'UTCTAI', 'U1', STATUS )
      CALL VVD ( U2, 0.8924826384444444444D0, 1D-12,&
                 'UTCTAI', 'U2', STATUS )
      CALL VIV ( J, 0, 'UTCTAI', 'J', STATUS )

      END

      SUBROUTINE T_UTCUT1 ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ U T C U T 1
!  - - - - - - - - - - - - -
!
!  Test UTCUT1 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  UTCUT1, VVD, VIV
!
!  This revision:  2010 September 5
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL UTCUT1 ( 2453750.5D0, 0.892100694D0, 0.3341D0,&
                        U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'UTCUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045608981481481D0, 1D-12,&
                 'UTCUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'UTCUT1', 'J', STATUS )

      END

      SUBROUTINE T_XY06 ( STATUS )
!+
!  - - - - - - - - - - -
!   T _ i a u _ X Y 0 6
!  - - - - - - - - - - -
!
!  Test XY06 routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  XY06, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y


      CALL XY06 ( 2400000.5D0, 53736D0, X, Y )

      CALL VVD ( X, 0.5791308486706010975D-3, 1D-15,&
                 'XY06', 'X', STATUS )
      CALL VVD ( Y, 0.4020579816732958141D-4, 1D-16,&
                 'XY06', 'Y', STATUS )

      END

      SUBROUTINE T_XYS00A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ X Y S 0 0 A
!  - - - - - - - - - - - - -
!
!  Test XYS00A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  XYS00A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, S


      CALL XYS00A ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791308472168152904D-3, 1D-14,&
                 'XYS00A', 'X', STATUS )
      CALL VVD ( Y, 0.4020595661591500259D-4, 1D-15,&
                 'XYS00A', 'Y', STATUS )
      CALL VVD ( S, -0.1220040848471549623D-7, 1D-18,&
                 'XYS00A', 'S', STATUS )

      END

      SUBROUTINE T_XYS00B ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ X Y S 0 0 B
!  - - - - - - - - - - - - -
!
!  Test XYS00B routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  XYS00B, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  X, Y, S


      CALL XYS00B ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791301929950208873D-3, 1D-14,&
                 'XYS00B', 'X', STATUS )
      CALL VVD ( Y, 0.4020553681373720832D-4, 1D-15,&
                 'XYS00B', 'Y', STATUS )
      CALL VVD ( S, -0.1220027377285083189D-7, 1D-18,&
                 'XYS00B', 'S', STATUS )

      END

      SUBROUTINE T_XYS06A ( STATUS )
!+
!  - - - - - - - - - - - - -
!   T _ i a u _ X Y S 0 6 A
!  - - - - - - - - - - - - -
!
!  Test XYS06A routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  XYS06A, VVD
!
!  This revision:  2009 July 11
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  X, Y, S


      CALL XYS06A ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791308482835292617D-3, 1D-14,&
                 'XYS06A', 'X', STATUS )
      CALL VVD ( Y, 0.4020580099454020310D-4, 1D-15,&
                 'XYS06A', 'Y', STATUS )
      CALL VVD ( S, -0.1220032294164579896D-7, 1D-18,&
                 'XYS06A', 'S', STATUS )

      END

      SUBROUTINE T_ZP ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ Z P
!  - - - - - - - - -
!
!  Test ZP routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ZP, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL ZP ( P )

      CALL VVD ( P(1), 0D0, 0D0, 'ZP', '1', STATUS )
      CALL VVD ( P(2), 0D0, 0D0, 'ZP', '2', STATUS )
      CALL VVD ( P(3), 0D0, 0D0, 'ZP', '3', STATUS )

      END

      SUBROUTINE T_ZPV ( STATUS )
!+
!  - - - - - - - - - -
!   T _ i a u _ Z P V
!  - - - - - - - - - -
!
!  Test ZPV routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ZPV, VVD
!
!  This revision:  2008 November 29
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL ZPV ( PV )

      CALL VVD ( PV(1,1), 0D0, 0D0, 'ZPV', 'P1', STATUS )
      CALL VVD ( PV(2,1), 0D0, 0D0, 'ZPV', 'P2', STATUS )
      CALL VVD ( PV(3,1), 0D0, 0D0, 'ZPV', 'P3', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0, 'ZPV', 'V1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0, 'ZPV', 'V2', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0, 'ZPV', 'V3', STATUS )

      END

      SUBROUTINE T_ZR ( STATUS )
!+
!  - - - - - - - - -
!   T _ i a u _ Z R
!  - - - - - - - - -
!
!  Test ZR routine.
!
!  Returned:
!     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
!
!  Called:  ZR, VVD
!
!  This revision:  2008 November 30
!-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  R(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL ZR ( R )

      CALL VVD ( R(1,1), 0D0, 0D0, 'ZR', '11', STATUS )
      CALL VVD ( R(1,2), 0D0, 0D0, 'ZR', '12', STATUS )
      CALL VVD ( R(1,3), 0D0, 0D0, 'ZR', '13', STATUS )
      CALL VVD ( R(2,1), 0D0, 0D0, 'ZR', '21', STATUS )
      CALL VVD ( R(2,2), 0D0, 0D0, 'ZR', '22', STATUS )
      CALL VVD ( R(2,3), 0D0, 0D0, 'ZR', '23', STATUS )
      CALL VVD ( R(3,1), 0D0, 0D0, 'ZR', '31', STATUS )
      CALL VVD ( R(3,2), 0D0, 0D0, 'ZR', '32', STATUS )
      CALL VVD ( R(3,3), 0D0, 0D0, 'ZR', '33', STATUS )

!-----------------------------------------------------------------------

!+----------------------------------------------------------------------
!
!  Copyright (C) 2019
!  Standards Of Fundamental Astronomy Board
!  of the International Astronomical Union.
!
!  =====================
!  SOFA Software License
!  =====================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
!  CONDITIONS WHICH APPLY TO ITS USE.
!
!  1. The Software is owned by the IAU SOFA Board ("SOFA").
!
!  2. Permission is granted to anyone to use the SOFA software for any
!     purpose, including commercial applications, free of charge and
!     without payment of royalties, subject to the conditions and
!     restrictions listed below.
!
!  3. You (the user) may copy and distribute SOFA source code to others,
!     and use and adapt its code and algorithms in your own software,
!     on a world-wide, royalty-free basis.  That portion of your
!     distribution that does not consist of intact and unchanged copies
!     of SOFA source code files is a "derived work" that must comply
!     with the following requirements:
!
!     a) Your work shall be marked or carry a statement that it
!        (i) uses routines and computations derived by you from
!        software provided by SOFA under license to you; and
!        (ii) does not itself constitute software provided by and/or
!        endorsed by SOFA.
!
!     b) The source code of your derived work must contain descriptions
!        of how the derived work is based upon, contains and/or differs
!        from the original SOFA software.
!
!     c) The names of all routines in your derived work shall not
!        include the prefix "iau" or "sofa" or trivial modifications
!        thereof such as changes of case.
!
!     d) The origin of the SOFA components of your derived work must
!        not be misrepresented;  you must not claim that you wrote the
!        original software, nor file a patent application for SOFA
!        software or algorithms embedded in the SOFA software.
!
!     e) These requirements must be reproduced intact in any source
!        distribution and shall apply to anyone to whom you have
!        granted a further right to modify the source code of your
!        derived work.
!
!     Note that, as originally distributed, the SOFA software is
!     intended to be a definitive implementation of the IAU standards,
!     and consequently third-party modifications are discouraged.  All
!     variations, no matter how minor, must be explicitly marked as
!     such, as explained above.
!
!  4. You shall not cause the SOFA software to be brought into
!     disrepute, either by misuse, or use for inappropriate tasks, or
!     by inappropriate modification.
!
!  5. The SOFA software is provided "as is" and SOFA makes no warranty
!     as to its use or performance.   SOFA does not and cannot warrant
!     the performance or results which the user may obtain by using the
!     SOFA software.  SOFA makes no warranties, express or implied, as
!     to non-infringement of third party rights, merchantability, or
!     fitness for any particular purpose.  In no event will SOFA be
!     liable to the user for any consequential, incidental, or special
!     damages, including any lost profits or lost savings, even if a
!     SOFA representative has been advised of such damages, or for any
!     claim by any third party.
!
!  6. The provision of any version of the SOFA software under the terms
!     and conditions specified herein does not imply that future
!     versions will also be made available under the same terms and
!     conditions.
!
!  In any published work or commercial product which uses the SOFA
!  software directly, acknowledgement (see www.iausofa.org) is
!  appreciated.
!
!  Correspondence concerning SOFA software should be addressed as
!  follows:
!
!      By email:  sofa@ukho.gov.uk
!      By post:   IAU SOFA Center
!                 HM Nautical Almanac Office
!                 UK Hydrographic Office
!                 Admiralty Way, Taunton
!                 Somerset, TA1 2DN
!                 United Kingdom
!
!-----------------------------------------------------------------------

      END

      END PROGRAM
