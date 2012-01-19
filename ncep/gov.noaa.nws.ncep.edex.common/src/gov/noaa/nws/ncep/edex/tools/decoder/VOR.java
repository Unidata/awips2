/**
 *
 */
package gov.noaa.nws.ncep.edex.tools.decoder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//import com.raytheon.edex.tools.decoder.LatLonPoint;      //TO10
import com.raytheon.uf.edex.decodertools.core.LatLonPoint; //TO11

/**
 *  VOR - A *TEMPORARY* enum class to define some known VORs
 *  used to define convective SIGMET locations.  (See below.)
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Jun 2009  95         B. Hebbard  Initial creation.
 * 24 Jun 2009  95/132     B. Hebbard  Add getLatLonPoint; move to common plugin
 * 22 Jul 2009  for 132    B. Hebbard  Port to TO11 until station table avail. EDEX
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public enum VOR {
	
	//  (From GEMPAK vors.tbl -- for TEMPORARY use only, just until
	//   this is handled by more general station/location design.
	//
	//   Note these are a SUBSET of the high-altitude VORs in North America
	//   used by AWC for SIGMET bounding points.  It is NOT sufficient
	//   for lookup in cases where where all VHF NAVAIDs -- or
	//   even all VORs -- are required [say, for PIREP decoding])
	//
	//            Lat       Lon
	YSJ       (  45.32 ,  -65.88 ) ,  
	HUL       (  46.04 ,  -67.83 ) ,  
	PQI       (  46.77 ,  -68.09 ) ,  
	MLT       (  45.58 ,  -68.52 ) ,  
	BGR       (  44.84 ,  -68.87 ) ,  
	ACK       (  41.28 ,  -70.03 ) ,  
	ENE       (  43.43 ,  -70.61 ) ,  
	BOS       (  42.36 ,  -70.99 ) ,  
	YQB       (  46.80 ,  -71.38 ) ,  
	PVD       (  41.72 ,  -71.43 ) ,  
	CON       (  43.22 ,  -71.58 ) ,  
	YSC       (  45.43 ,  -71.68 ) ,  
	HTO       (  40.92 ,  -72.32 ) ,  
	MPV       (  44.22 ,  -72.57 ) ,  
	BDL       (  41.94 ,  -72.69 ) ,  
	PLB       (  44.69 ,  -73.52 ) ,  
	JFK       (  40.63 ,  -73.77 ) ,  
	ALB       (  42.75 ,  -73.80 ) ,  
	CYN       (  39.82 ,  -74.43 ) ,  
	SAX       (  41.07 ,  -74.54 ) ,  
	MSS       (  44.91 ,  -74.72 ) ,  
	SIE       (  39.10 ,  -74.80 ) ,  
	HNK       (  42.06 ,  -75.32 ) ,  
	SBY       (  38.35 ,  -75.52 ) ,  
	YOW       (  45.32 ,  -75.67 ) ,  
	ETX       (  40.58 ,  -75.68 ) ,  
	ECG       (  36.25 ,  -76.18 ) ,  
	SYR       (  43.16 ,  -76.20 ) ,  
	ORF       (  36.89 ,  -76.20 ) ,  
	EMI       (  39.50 ,  -76.98 ) ,  
	HAR       (  40.23 ,  -77.02 ) ,  
	DCA       (  38.86 ,  -77.04 ) ,  
	RIC       (  37.50 ,  -77.32 ) ,  
	CSN       (  38.64 ,  -77.87 ) ,  
	ILM       (  34.35 ,  -77.87 ) ,  
	SLT       (  41.51 ,  -77.97 ) ,  
	PSB       (  40.92 ,  -77.99 ) ,  
	BUF       (  42.93 ,  -78.65 ) ,  
	RDU       (  35.87 ,  -78.78 ) ,  
	JST       (  40.32 ,  -78.83 ) ,  
	JHW       (  42.19 ,  -79.12 ) ,  
	LYH       (  37.25 ,  -79.23 ) ,  
	YYZ       (  43.67 ,  -79.63 ) ,  
	FLO       (  34.23 ,  -79.66 ) ,  
	GSO       (  36.05 ,  -79.98 ) ,  
	CHS       (  32.89 ,  -80.04 ) ,  
	PBI       (  26.68 ,  -80.09 ) ,  
	EKN       (  38.92 ,  -80.10 ) ,  
	EWC       (  40.83 ,  -80.21 ) ,  
	ERI       (  42.02 ,  -80.30 ) ,  
	MIA       (  25.80 ,  -80.30 ) ,  
	VRB       (  27.68 ,  -80.49 ) ,  
	PSK       (  37.09 ,  -80.71 ) ,  
	AIR       (  40.02 ,  -80.82 ) ,  
	CLT       (  35.22 ,  -80.93 ) ,  
	CAE       (  33.86 ,  -81.05 ) ,  
	YVV       (  44.75 ,  -81.10 ) ,  
	SAV       (  32.16 ,  -81.11 ) ,  
	OMN       (  29.30 ,  -81.11 ) ,  
	BKW       (  37.78 ,  -81.12 ) ,  
	ORL       (  28.54 ,  -81.34 ) ,  
	CRG       (  30.34 ,  -81.51 ) ,  
	EYW       (  24.59 ,  -81.80 ) ,  
	FMY       (  26.58 ,  -81.87 ) ,  //  OBSOLETE  
	SPA       (  35.03 ,  -81.93 ) ,  
	HNN       (  38.75 ,  -82.03 ) ,  
	HMV       (  36.44 ,  -82.13 ) ,  
	CLE       (  41.42 ,  -81.85 ) ,  
	IRQ       (  33.71 ,  -82.16 ) ,  
	AMG       (  31.54 ,  -82.51 ) ,  
	SRQ       (  27.40 ,  -82.55 ) ,  
	APE       (  40.15 ,  -82.59 ) ,  
	PIE       (  27.91 ,  -82.68 ) ,  
	ECK       (  43.26 ,  -82.72 ) ,  
	CTY       (  29.60 ,  -83.05 ) ,  
	ODF       (  34.70 ,  -83.30 ) ,  
	DXO       (  42.21 ,  -83.37 ) ,  
	ASP       (  44.45 ,  -83.39 ) ,  
	MCN       (  32.69 ,  -83.65 ) ,  
	FNT       (  42.97 ,  -83.74 ) ,  
	VXV       (  35.90 ,  -83.89 ) ,  
	ROD       (  40.29 ,  -84.04 ) ,  
	MBS       (  43.53 ,  -84.08 ) ,  
	LOZ       (  37.03 ,  -84.12 ) ,  
	ABY       (  31.65 ,  -84.30 ) ,  //  OBSOLETE  
	SSM       (  46.41 ,  -84.31 ) ,  
	TLH       (  30.56 ,  -84.37 ) ,  
	ATL       (  33.63 ,  -84.44 ) ,  
	CVG       (  39.02 ,  -84.70 ) ,  
	GQO       (  34.96 ,  -85.15 ) ,  
	FWA       (  40.98 ,  -85.19 ) ,  
	LGC       (  33.05 ,  -85.21 ) ,  
	GRR       (  42.79 ,  -85.50 ) ,  
	TVC       (  44.67 ,  -85.55 ) ,  
	LOU       (  38.10 ,  -85.58 ) ,  //  OBSOLETE  
	MKG       (  43.17 ,  -86.04 ) ,  
	PMM       (  42.47 ,  -86.11 ) ,  
	GIJ       (  41.77 ,  -86.32 ) ,  
	MGM       (  32.22 ,  -86.32 ) ,  
	IND       (  39.81 ,  -86.37 ) ,  
	BWG       (  36.93 ,  -86.44 ) ,  
	BNA       (  36.14 ,  -86.68 ) ,  
	CEW       (  30.83 ,  -86.68 ) ,  
	VUZ       (  33.67 ,  -86.90 ) ,  
	BVT       (  40.56 ,  -87.07 ) ,  
	TTH       (  39.49 ,  -87.25 ) ,  
	MSL       (  34.70 ,  -87.48 ) ,  
	SAW       (  46.36 ,  -87.40 ) ,  
	PXV       (  37.93 ,  -87.76 ) ,  
	ORD       (  41.98 ,  -87.90 ) ,  
	GRB       (  44.56 ,  -88.19 ) ,  
	BAE       (  43.12 ,  -88.28 ) ,  
	JOT       (  41.55 ,  -88.32 ) ,  
	SJI       (  30.73 ,  -88.36 ) ,  
	IGB       (  33.48 ,  -88.52 ) ,  
	MEI       (  32.38 ,  -88.80 ) ,  
	DEC       (  39.74 ,  -88.86 ) ,  
	YQT       (  48.37 ,  -89.32 ) ,  
	DYR       (  36.02 ,  -89.32 ) ,  
	RHI       (  45.63 ,  -89.45 ) ,  
	BDF       (  41.16 ,  -89.59 ) ,  
	DLL       (  43.55 ,  -89.76 ) ,  
	MEM       (  35.06 ,  -89.98 ) ,  
	LEV       (  29.18 ,  -90.10 ) ,  
	JAN       (  32.51 ,  -90.17 ) ,  
	MSY       (  30.00 ,  -90.27 ) ,  //  OBSOLETE  
	FAM       (  37.67 ,  -90.23 ) ,  
	MCB       (  31.30 ,  -90.26 ) ,  
	SQS       (  33.46 ,  -90.28 ) ,  
	STL       (  38.86 ,  -90.48 ) ,  
	DBQ       (  42.40 ,  -90.71 ) ,  
	ARG       (  36.11 ,  -90.95 ) ,  
	UIN       (  39.85 ,  -91.28 ) ,  
	BTR       (  30.48 ,  -91.30 ) ,  
	ODI       (  43.91 ,  -91.47 ) ,  
	EAU       (  44.90 ,  -91.48 ) ,  
	IOW       (  41.52 ,  -91.61 ) ,  
	MLU       (  32.52 ,  -92.03 ) ,  
	LIT       (  34.68 ,  -92.18 ) ,  
	DLH       (  46.80 ,  -92.20 ) ,  
	COU       (  38.82 ,  -92.22 ) ,  
	AEX       (  31.26 ,  -92.50 ) ,  
	IRK       (  40.14 ,  -92.59 ) ,  
	ELD       (  33.26 ,  -92.74 ) ,  
	LCH       (  30.14 ,  -93.11 ) ,  
	MSP       (  44.88 ,  -93.23 ) ,  
	MCW       (  43.09 ,  -93.33 ) ,  
	SGF       (  37.36 ,  -93.33 ) ,  
	INL       (  48.57 ,  -93.40 ) ,  
	DSM       (  41.44 ,  -93.65 ) ,  
	EIC       (  32.77 ,  -93.81 ) ,  
	BRD       (  46.35 ,  -94.03 ) ,  
	TXK       (  33.51 ,  -94.07 ) ,  
	RZC       (  36.25 ,  -94.12 ) ,  
	FSM       (  35.38 ,  -94.27 ) ,  
	FOD       (  42.61 ,  -94.29 ) ,  
	BUM       (  38.27 ,  -94.49 ) ,  
	MKC       (  39.28 ,  -94.59 ) ,  //  OBSOLETE  
	LFK       (  31.16 ,  -94.72 ) ,  
	GGG       (  32.42 ,  -94.75 ) ,  
	BJI       (  47.58 ,  -95.02 ) ,  
	RWF       (  44.47 ,  -95.13 ) ,  
	OSW       (  37.15 ,  -95.20 ) ,  
	IAH       (  29.96 ,  -95.35 ) ,  
	OVR       (  41.17 ,  -95.74 ) ,  
	MLC       (  34.85 ,  -95.78 ) ,  
	TUL       (  36.20 ,  -95.79 ) ,  
	PWE       (  40.20 ,  -96.21 ) ,  
	PSX       (  28.76 ,  -96.31 ) ,  
	FSD       (  43.65 ,  -96.78 ) ,  
	FAR       (  46.75 ,  -96.85 ) ,  
	DFW       (  32.87 ,  -97.03 ) ,  //  OBSOLETE  
	ADM       (  34.21 ,  -97.17 ) ,  
	GFK       (  47.95 ,  -97.19 ) ,  
	YWG       (  49.90 ,  -97.23 ) ,  
	ACT       (  31.66 ,  -97.27 ) ,  
	BRO       (  25.92 ,  -97.38 ) ,  
	CRP       (  27.90 ,  -97.45 ) ,  
	ICT       (  37.75 ,  -97.58 ) ,  
	OKC       (  35.36 ,  -97.61 ) ,  
	SLN       (  38.93 ,  -97.62 ) ,  
	AUS       (  30.30 ,  -97.70 ) ,  //  OBSOLETE  
	END       (  36.35 ,  -97.92 ) ,  
	OBH       (  41.38 ,  -98.35 ) ,  
	ABR       (  45.42 ,  -98.37 ) ,  
	SAT       (  29.64 ,  -98.46 ) ,  
	SPS       (  33.99 ,  -98.59 ) ,  
	ONL       (  42.47 ,  -98.69 ) ,  
	LRD       (  27.48 ,  -99.42 ) ,  
	JCT       (  30.60 ,  -99.82 ) ,  
	ABI       (  32.48 ,  -99.86 ) ,  
	GAG       (  36.34 ,  -99.88 ) ,  
	ANW       (  42.57 ,  -99.99 ) ,  
	PIR       (  44.40 , -100.17 ) ,  
	HLC       (  39.26 , -100.23 ) ,  
	CDS       (  34.37 , -100.28 ) ,  
	SJT       (  31.38 , -100.46 ) ,  
	MCK       (  40.20 , -100.59 ) ,  
	BIS       (  46.77 , -100.67 ) ,  
	LBF       (  41.13 , -100.72 ) ,  
	GCK       (  37.92 , -100.73 ) ,  
	DLF       (  29.36 , -100.77 ) ,  
	LBL       (  37.04 , -100.97 ) ,  
	MOT       (  48.26 , -101.29 ) ,  
	AMA       (  35.29 , -101.64 ) ,  
	GLD       (  39.39 , -101.69 ) ,  
	DPR       (  45.08 , -101.72 ) ,  
	LBB       (  33.70 , -101.92 ) ,  
	MAF       (  32.02 , -102.18 ) ,  
	LAA       (  38.20 , -102.69 ) ,  
	DIK       (  46.86 , -102.77 ) ,  
	TXO       (  34.50 , -102.84 ) ,  
	SNY       (  41.10 , -102.98 ) ,  
	FST       (  30.95 , -102.98 ) ,  
	RAP       (  43.98 , -103.01 ) ,  
	AKO       (  40.16 , -103.18 ) ,  
	INK       (  31.87 , -103.24 ) ,  
	BFF       (  41.89 , -103.48 ) ,  
	TBE       (  37.27 , -103.60 ) ,  
	TCC       (  35.18 , -103.60 ) ,  
	ISN       (  48.18 , -103.63 ) ,  
	MRF       (  30.30 , -103.95 ) ,  
	PUB       (  38.29 , -104.43 ) ,  
	ROW       (  33.34 , -104.62 ) ,  //  OBSOLETE  
	DEN       (  39.81 , -104.66 ) ,  
	CYS       (  41.21 , -104.77 ) ,  
	CIM       (  36.49 , -104.87 ) ,  
	LVS       (  35.66 , -105.14 ) ,  //  OBSOLETE  
	LAR       (  41.33 , -105.72 ) ,  
	ALS       (  37.35 , -105.82 ) ,  
	MLS       (  46.38 , -105.95 ) ,  
	DDY       (  43.09 , -106.28 ) ,  
	ELP       (  31.82 , -106.28 ) ,  
	CZI       (  44.00 , -106.44 ) ,  
	GGW       (  48.22 , -106.63 ) ,  
	ABQ       (  35.04 , -106.82 ) ,  
	DBL       (  39.44 , -106.90 ) ,  
	HBU       (  38.45 , -107.04 ) ,  
	SHR       (  44.84 , -107.06 ) ,  
	TCS       (  33.28 , -107.28 ) ,  
	CHE       (  40.52 , -107.31 ) ,  
	DMN       (  32.28 , -107.60 ) ,  
	YYN       (  50.28 , -107.68 ) ,  
	FMN       (  36.75 , -108.10 ) ,  //  OBSOLETE  
	BOY       (  43.46 , -108.30 ) ,  
	BIL       (  45.81 , -108.63 ) ,  
	JNC       (  39.06 , -108.79 ) ,  
	DVC       (  37.81 , -108.93 ) ,  
	OCS       (  41.59 , -109.02 ) ,  
	SJN       (  34.42 , -109.14 ) ,  
	SSO       (  32.27 , -109.26 ) ,  
	LWT       (  47.05 , -109.61 ) ,  
	HVR       (  48.54 , -109.77 ) ,  
	BPI       (  42.58 , -110.11 ) ,  
	MTU       (  40.15 , -110.13 ) ,  
	HVE       (  38.42 , -110.70 ) ,  
	YXH       (  50.02 , -110.72 ) ,  
	JAC       (  43.62 , -110.73 ) ,  
	INW       (  35.06 , -110.80 ) ,  
	TUS       (  32.10 , -110.92 ) ,  
	TBC       (  36.12 , -111.27 ) ,  
	GTF       (  47.45 , -111.41 ) ,  
	HLN       (  46.61 , -111.95 ) ,  
	PHX       (  33.43 , -112.02 ) ,  
	SLC       (  40.85 , -111.98 ) ,  
	DBS       (  44.09 , -112.21 ) ,  
	BCE       (  37.69 , -112.30 ) ,  
	MLD       (  42.20 , -112.45 ) ,  
	DRK       (  34.70 , -112.48 ) ,  
	DTA       (  39.30 , -112.51 ) ,  
	DLN       (  45.25 , -112.55 ) ,  
	PIH       (  42.87 , -112.65 ) ,  
	YQL       (  49.63 , -112.80 ) ,  
	PGS       (  35.62 , -113.54 ) ,  
	BVL       (  40.73 , -113.76 ) ,  
	LKT       (  45.02 , -114.08 ) ,  
	FCA       (  48.21 , -114.18 ) ,  
	ILC       (  38.25 , -114.39 ) ,  
	EED       (  34.77 , -114.47 ) ,  
	TWF       (  42.48 , -114.49 ) ,  
	BZA       (  32.77 , -114.60 ) ,  
	ELY       (  39.30 , -114.85 ) ,  
	LAS       (  36.08 , -115.16 ) ,  
	MLP       (  47.46 , -115.65 ) ,  
	YXC       (  49.60 , -115.78 ) ,  
	TRM       (  33.63 , -116.16 ) ,  
	BOI       (  43.55 , -116.19 ) ,  
	DNJ       (  44.77 , -116.21 ) ,  
	HEC       (  34.80 , -116.46 ) ,  
	BTY       (  36.80 , -116.75 ) ,  
	BAM       (  40.57 , -116.92 ) ,  
	MZB       (  32.78 , -117.23 ) ,  
	GEG       (  47.56 , -117.63 ) ,  
	OAL       (  38.00 , -117.77 ) ,  
	BKE       (  44.84 , -117.81 ) ,  
	REO       (  42.59 , -117.87 ) ,  
	LAX       (  33.93 , -118.43 ) ,  
	PDT       (  45.70 , -118.94 ) ,  
	EHF       (  35.48 , -119.10 ) ,  
	EPH       (  47.38 , -119.42 ) ,  
	FMG       (  39.53 , -119.66 ) ,  
	RZS       (  34.51 , -119.77 ) ,  
	CZQ       (  36.88 , -119.82 ) ,  
	YKM       (  46.57 , -120.45 ) ,  
	LKV       (  42.49 , -120.51 ) ,  
	YDC       (  49.47 , -120.52 ) ,  
	MOD       (  37.63 , -120.96 ) ,  
	DSD       (  44.25 , -121.30 ) ,  
	SAC       (  38.44 , -121.55 ) ,  
	SNS       (  36.66 , -121.60 ) ,  
	OAK       (  37.73 , -122.22 ) ,  
	RBL       (  40.10 , -122.24 ) ,  
	SEA       (  47.44 , -122.31 ) ,  
	BLI       (  48.95 , -122.58 ) ,  //  OBSOLETE  
	PDX       (  45.58 , -122.60 ) ,  
	PYE       (  38.08 , -122.87 ) ,  
	OED       (  42.48 , -122.91 ) ,  
	EUG       (  44.12 , -123.22 ) ,  
	ENI       (  39.05 , -123.27 ) ,  
	ONP       (  44.58 , -124.06 ) ,  
	HQM       (  46.95 , -124.15 ) ,  
	FOT       (  40.67 , -124.23 ) ,  
	TOU       (  48.30 , -124.63 ) ,  
	YQV       (  51.27 , -102.47 ) ,  
	ANN       (  55.05 , -131.57 ) ,  
	LVD       (  56.47 , -133.08 ) ,  
	BKA       (  56.86 , -135.55 ) ,  
	SSR       (  58.17 , -135.25 ) ,  
	JNU       (  58.35 , -134.58 ) ,  
	YAK       (  59.50 , -139.67 ) ,  
	MDO       (  59.45 , -146.30 ) ,  
	JOH       (  60.48 , -146.60 ) ,  
	ODK       (  57.75 , -152.50 ) ,  
	HOM       (  59.65 , -151.48 ) ,  
	ENA       (  60.57 , -151.25 ) ,  
	ANC       (  61.17 , -150.00 ) ,  
	BGQ       (  61.53 , -149.82 ) ,  
	ORT       (  62.97 , -141.93 ) ,  
	GKN       (  62.15 , -145.45 ) ,  
	TKA       (  62.32 , -150.10 ) ,  
	SQA       (  61.10 , -155.63 ) ,  
	DLG       (  59.05 , -158.50 ) ,  
	AKN       (  58.68 , -156.65 ) ,  
	PDN       (  56.95 , -158.65 ) ,  
	CDB       (  55.20 , -162.73 ) ,  
	DUT       (  53.90 , -166.55 ) ,  
	NUD       (  51.88 , -176.65 ) ,  
	SYA       (  52.72 , -174.12 ) ,  
	SPY       (  57.17 , -170.22 ) ,  
	EHM       (  58.66 , -162.07 ) ,  
	HPB       (  61.52 , -166.14 ) ,  
	BET       (  60.78 , -161.83 ) ,  
	ANI       (  61.59 , -159.61 ) ,  
	SMA       (  62.06 , -163.30 ) ,  
	UNK       (  63.88 , -160.80 ) ,  
	ULL       (  63.70 , -170.48 ) ,  
	MCG       (  62.95 , -155.60 ) ,  
	ENN       (  64.55 , -149.07 ) ,  
	FAI       (  64.82 , -147.85 ) ,  
	BIG       (  64.00 , -145.72 ) ,  
	FYU       (  66.57 , -145.25 ) ,  
	BTT       (  66.92 , -151.53 ) ,  
	TAL       (  65.18 , -152.18 ) ,  
	CQR       (  67.50 , -148.47 ) ,  
	SCC       (  70.20 , -148.47 ) ,  
	BTI       (  70.13 , -143.57 ) ,  
	BRW       (  71.28 , -156.77 ) ,  
	GAL       (  64.73 , -156.93 ) ,  
	OME       (  64.52 , -165.45 ) ,  
	OTZ       (  66.88 , -162.60 ) ,  
	WLK       (  66.60 , -160.00 ) ,  
	HSL       (  65.71 , -156.37 ) ,  
	BSF       (  19.76 , -155.39 ) ,  
	UPP       (  20.20 , -155.84 ) ,  
	ITO       (  19.72 , -155.01 ) ,  
	HNL       (  21.33 , -157.93 ) ,  
	OGG       (  20.91 , -156.42 ) ,  
	NDB       (  20.88 , -156.44 ) ,  
	MUE       (  20.00 , -155.67 ) ,  
	NGF       (  21.45 , -157.76 ) ,  
	MKK       (  21.14 , -157.17 ) ,  
	NBS       (  22.04 , -159.79 ) ,  
	CKH       (  21.27 , -157.70 ) ,  
	IAI       (  19.65 , -156.02 ) ,  
	LLD       (  20.77 , -156.97 ) ,  
	LNY       (  20.76 , -156.97 ) ,  
	LIH       (  21.97 , -159.34 ) ,  
	SOK       (  21.90 , -159.53 ) ,
	//
	//  Newcomers!  These are in the set of VORs now used by AWC for Convective SIGMET bounds,
	//  but not in (out-of-the-box V5.11.4) $GEMTBL/stns/vors.tbl :
	//
	RSW       (  26.53 ,  -81.78 ) ,  //  Lee County VORTAC   Fort Myers FL         L-VORTAC !?    replaces FMY
	PZD       (  31.66 ,  -84.29 ) ,  //  Pecan VORTAC        Albany GA             H-VORTACW      replaces ABY
	IIU       (  38.10 ,  -85.58 ) ,  //  Louisville VORTAC   Louisville KY         H-VORTAC       replaces LOU
	HRV       (  29.85 ,  -90.00 ) ,  //  Harvey VORTAC       New Orleans LA        H-VORTACW      replaces MSY
	MCI       (  39.29 ,  -94.74 ) ,  //  Kansas City VORTAC  Kansas City MO        H-VORTAC       replaces MKC
	TTT       (  32.87 ,  -97.04 ) ,  //  Maverick VOR/DME    Dallas-Fort Worth TX  H-VORW/DME     replaces DFW
	CWK       (  30.38 ,  -97.53 ) ,  //  Centex VORTAC       Austin TX             H-VORTACW      replaces AUS
	CME       (  33.34 , -104.62 ) ,  //  Chisum VORTAC       Roswell NM            H-VORTACW      replaces ROW
	FTI       (  35.66 , -105.14 ) ,  //  Fort Union VORTAC   Las Vegas NM          H-VORTACW      replaces LVS
	RSK       (  36.75 , -108.10 ) ,  //  Rattlesnake VORTAC  Farmington NM         H-VORTACW      replaces FMN
	HUH       (  48.95 , -122.58 ) ;  //  Whatcom VORTAC      Bellingham WA         H-VORTACW      replaces BLI

    private static Log logger = LogFactory.getLog(VOR.class);

	private double latitude;
	private double longitude;
	
	private VOR (double latitude, double longitude)
	{
		this.latitude = latitude;
		this.longitude = longitude;
	}
	
	public double getLatitude()
	{
		return latitude;
	}
	
	public double getLongitude()
	{
		return longitude;
	}

	public LatLonPoint getLatLonPoint()
	{
		return new LatLonPoint (latitude, longitude, LatLonPoint.INDEGREES);
	}
    
    private enum Direction {
    	N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW;
    	public double getDegrees() { return ordinal() * 22.5; }
    }

    private static final double ONE_NM_RADIANS = Math.toRadians (1.0 / 60.0);
    
    /**
     * Given a VOR-relative reference string, returns a LatLonPoint
     * (com.raytheon.uf.edex.decodertools.core.LatLonPoint).
     * 
     * @param location A String such as...
     *                 "BOS"
     *                 "20S EMI"
     *                 "30 WNW BUM"
     *                 " 40ENE HUH "
     *                 ...referencing a VOR listed in AC 00-45F
     *                 (Appendix F), optionally preceded by
     *                 distance in nautical miles and 16-point
     *                 compass direction string.
     * @return         The decoded location as a LatLonPoint;
     *                 null on error (such as unrecognized VOR
     *                 identifier or direction string).
     *
     */
    public static LatLonPoint getLatLonPoint(String location) {
    	//  Wrap decoding in a try block, in case of exception on
    	//  one ofthe two enum valueOf lookups, or other problems.
    	try {
    		location = location.trim();
    	    //  VOR is always last 3 nonblank char of location
    		String navaid = location.substring(location.length()-3);
    		LatLonPoint point = VOR.valueOf(navaid).getLatLonPoint();
    		//  If there's an offset direction/bearing, process it
    		if (location.length() > 3) {
    			String u = location.substring(0, location.length()-3);
    			Pattern p = Pattern.compile("^([0-9]+)\\s*([A-Z]+)");
    			Matcher m = p.matcher(u);
    			if (m.find()) {
    				String distanceStr = m.group(1);
    				String bearingStr  = m.group(2);
    				int distanceNM = Integer.parseInt(distanceStr);
    				double distanceRad = distanceNM * ONE_NM_RADIANS;
    				//  LatLonPoint.positionOf thinks bearing is CCW, not CW...
    				double bearingDeg = 360.0 - Direction.valueOf(bearingStr).getDegrees();
    				double bearingRad = Math.toRadians(bearingDeg);
    				point = point.positionOf(bearingRad, distanceRad);
    			}
    		}
    		return point;
    	}
    	catch (Exception e) {
    	    logger.error("[Error decoding location:  " + location + "]");
    		return null;
    	}
	}

}
