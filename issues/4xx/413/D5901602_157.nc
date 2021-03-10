CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   j   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-05-21T12:58:15Z creation;2015-04-24T04:04:12Z conversion to V3.1;2017-08-18T07:48:12Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  ;D   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  =X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  ?l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  A�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  C�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  F   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Id   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   K�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   T�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ]�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  f�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    gl   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    g|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    g�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         g�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         g�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        g�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    g�Argo profile    3.1 1.2 19500101000000  20130521125815  20170907061506  5901602 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  A14_86529_157                   2C  D   APEX                            4070                            062608                          846 @֛���� 1   @֛�8Q�@��S��@cK"��`B1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A#33AnffA���A�  A�33B
ffB ��B333BI33B[33Bp��B���B���B���B���B�33B�  B���B�ffB�  B�33B晚B�ffB�ffC�3C33C��C�fCL�CffC��C%L�C)� C/  C4� C8�fC<L�CC� CHL�CQ��C[�3Cf  CpL�Cz33C��3C�@ C�  C��fC�&fC�  C�ٚC��C���C�@ C��3C�33C��fC�L�C��C�L�C�@ C��C��fC�33C���C�3C���C�ٚC�ٚD  DfD�D&fD  D�3D   D$�3D)S3D/  D3��D8�3D>  DC  DG��DM�DQl�DW�D\�Da�De��Dk�Do��Du�Dy�3D�#3D�vfD���D���D�L�D�� D��3D�	�D�9�D�  D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33A,��Ax  A�ffA���A�  B��B#33B5��BK��B]��Bs33B�  B�  B�  B���B�ffB�33B���Bə�B�33B�ffB���B�B���CL�C��CffC� C�fC  C ffC%�fC*�C/��C5�C9� C<�fCD�CH�fCRffC\L�Cf��Cp�fCz��C�@ C���C�L�C�33C�s3C�L�C�&fC�Y�C��C���C�@ C�� C��3CÙ�C�Y�C͙�CҌ�C�Y�C�33C� C��C�  C��C�&fC�&fD&fD,�D@ DL�D&fD�D FfD%�D)y�D/FfD4  D9�D>FfDC&fDH  DM33DQ�3DW33D\33Da@ Df  Dk33Dp  Du33Dz�D�6fD���D���D�  D�` D��3D��fD��D�L�D�33D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�"�A��;A�-A�-A���A�wA�!A��A��A�A�RA��#A��A���A�ĜA蟾A�DA�\)A�\A���AڬA�z�A�l�A�hsA��`A���A���A�\)A���A��\A��jA���A���A���A���A��A���A`��AO�7A@jA9%A3�A+�A)VA%�^A$�DA!�-AȴA�A��A�`A�A�-AA�A�PA�\AdZA~�At�A��A
��A	l�A�TAI�A�uA�w@�1@���@���@�9X@�J@�@�=q@�I�@�\)@�(�@�ƨ@�I�@��;@�bN@�O�@�Q�@��R@�^5@�&�@��9@���@�n�@��P@��/@�$�@�"�@�/@��m@�o@}@t(�@g�;@_
=@V5?@PQ�@J^5@E@CC�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�"�A��;A�-A�-A���A�wA�!A��A��A�A�RA��#A��A���A�ĜA蟾A�DA�\)A�\A���AڬA�z�A�l�A�hsA��`A���A���A�\)A���A��\A��jA���A���A���A���A��A���A`��AO�7A@jA9%A3�A+�A)VA%�^A$�DA!�-AȴA�A��A�`A�A�-AA�A�PA�\AdZA~�At�A��A
��A	l�A�TAI�A�uA�w@�1@���@���@�9X@�J@�@�=q@�I�@�\)@�(�@�ƨ@�I�@��;@�bN@�O�@�Q�@��R@�^5@�&�@��9@���@�n�@��P@��/@�$�@�"�@�/@��m@�o@}@t(�@g�;@_
=@V5?@PQ�@J^5@E@CC�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B`BBbNBhsBo�B{�B�DB��B��B��B��B�!B�B�XB�XB��BȴB��B�;B��B	A�B	�
B
�mB;dB��B�3B��B�5B�B�wB�XB��B|�Bk�BXB=qB�B
�B
�jB
M�B
0!B
�B
,B
33B
YB
hsB
�B
�+B
�+B
�PB
�\B
�hB
�oB
�uB
�bB
�7B
�7B
�7B
�+B
�%B
�B
�B
~�B
}�B
{�B
x�B
u�B
q�B
jB
dZB
bNB
_;B
[#B
S�B
L�B
F�B
C�B
@�B
?}B
>wB
<jB
<jB
<jB
:^B
;dB
<jB
=qB
@�B
B�B
E�B
F�B
G�B
I�B
J�B
M�B
O�B
R�B
W
B
ZB
^5B
dZB
iyB
n�B
p�B
t�B
v�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BM�BO�BVB]IBi�Bx�B�dB��B��B��B��B��B�B�B�/B�`B�lB��B�B	2-B	͹B
��B-�B�B��B�.BΊBϑB��B��B�"Bm]B[�BH�B0�B0B
�@B
�+B
D�B
!�B
dB
�B
"�B
G�B
W$B
q�B
u�B
u�B
z�B
|�B
� B
�B
�B
~B
v�B
v�B
v�B
t�B
s�B
q�B
p�B
l�B
k�B
i�B
f�B
cnB
`\B
Y1B
RB
O�B
L�B
H�B
B�B
;B
4TB
1AB
./B
-)B
,"B
*B
*B
*B
(
B
)*B
*0B
+6B
/OB
0UB
3hB
4nB
5tB
7�B
8�B
;�B
=�B
@�B
D�B
G�B
K�B
R B
W?B
\]B
^jB
b�B
d�B
f�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%h�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<E�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=-0.6(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=0.018(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping; Use P>(dbar) Use P<(dbar) Use THETA<(deg.C) Use THETA>(deg.C)                                                                           Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201306031348312013060313483120130603134831201306031353292013060313532920130603135329201703061733572017030617335720170306173357  JA  ARFMdecpA14f                                                                20130521125757  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130521125815  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1                                                                 20130521125823  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20130521125823  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130521125824  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130521125828  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19c                                                                20130521125828  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130521125828  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8d                                                                20130521125828  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130521125829  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20130521132003                      G�O�G�O�G�O�                JA  ARFMdecpA14f                                                                20130524220018  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130524220556  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1                                                                 20130524220604  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20130524220605  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130524220606  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130524220610  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19c                                                                20130524220610  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130524220610  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8d                                                                20130524220610  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130524220611  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20130524221533                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424040412                      G�O�G�O�G�O�                JA  ARUP                                                                        20150502154510                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130603044831  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130603044831  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20130603045329  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20130621212006  CV  JULD_LOCATION   G�O�G�O�F��F                JM  ARGQJMQC2.0                                                                 20130621212006  CV  LATITUDE        G�O�G�O�@�/                JM  ARGQJMQC2.0                                                                 20130621212006  CV  LONGITUDE       G�O�G�O�CSu                JM  ARSQOW  1.1 CTD2017V1                                                       20170306083357  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20170907061506                      G�O�G�O�G�O�                