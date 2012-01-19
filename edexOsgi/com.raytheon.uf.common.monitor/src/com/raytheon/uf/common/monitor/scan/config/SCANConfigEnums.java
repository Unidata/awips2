/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.monitor.scan.config;

/**
 * Class containing common enumerations used in SCAN.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2009 #3039      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class SCANConfigEnums
{
    /**
     * Available SCAN tables.
     */
    public static enum ScanTables {CELL, DMD, MESO, TVS};
    
    /**
     * Threshold colors.
     */
    public static enum ScanThresholdColor {Upper, Mid, Lower, Default, Ident};
    
    public static enum WARN_TYPE {

        SEVERE("Severe Thunderstorm Warning"), TVS("Tornado Vortex Signature");

        private final String warntype;

        private WARN_TYPE(String name) {
            warntype = name;
        }

        public String getWarnType() {
            return warntype;
        }
    }; 
    
    /**
     * Common SCAN colors.
     */
    public static enum ScanColors {Attributes, CWAFilter, Rank, Unwarned,
        Configurations, LinkToFrame, Vert, Tips, Sort, ClutterControl, RadVar, Default, TVS};
    
    /**
     * CELL table enumeration.  Each identifier MUST match the column name in quotes if the
     * column name was changed to upper-case.
     * For example: AZM15("azm15")
     */
    public static enum CELLTable
    {
        // VCP not on CELL table     VCP("vcp")
        
        IDENT("ident"), AZM("azm"), RNG("rng"), TVS("tvs"), MDASR("mdaSR"),
        POSH("posh"), POH("poh"), HSIZE("hSize"), VIL("vil"), DBZ("dbz"), DBZHT("dbzHt"),
        TOP("top"), DIR("dir"), SPD("spd"), AZM15("azm15"), RNG15("rng15"), AZM30("azm30"),
        RNG30("rng30"), AZM45("azm45"), RNG45("rng45"), AZM60("azm60"), RNG60("rng60"), MVTERR("mvtErr"),
        MVTMN("mvtMn"), LAT("lat"), LON("lon"), POLH("polh"), SVRWX("svrwx"), HVYPR("hvyPr"),
        PPOS("pPos"), CGRATE("cgRate"), CAPE("cape"), SREH("sreh"), COUNTY("county"); 
        
        private String colName;
        
        CELLTable(String name)
        {
            colName = name;
        }
        
        public String getColName()
        {
            return colName;
        }
    };
    
    /**
     * DMD table enumeration.  Each identifier MUST match the column name in quotes if the
     * column name was changed to upper-case.
     * For example: LLCONV("llConv")
     */
    public static enum DMDTable
    {
        IDENT("ident"), AZM("azm"), RNG("rng"), STRANK("stRank"), STATUS("status"),
        CLASS("class"), MSI("msi"), TVS("tvs"), ELEV0("elev0"), BASE("base"), DEPTH("depth"), 
        RELDEP("relDep"), LLDIAM("llDiam"), LLVR("llVr"), MAXVR("maxVr"), HTMXVR("htMxVr"),
        LLSHR("llShr"), LLGTG("llgtg"), LLCONV("llConv"), MLCONV("mlConv"), DIR("dir"), SPD("spd"),
        AGE("age"), STRMID("strmID"), LAT("lat"), LON("lon"), COUNTY("county"), CWA("cwa");
        
        private String colName;
        
        DMDTable(String name)
        {
            colName = name;
        }
        
        public String getColName()
        {
            return colName;
        }
    };
    
    /**
     * MESO table enumeration.  Each identifier MUST match the column name in quotes if the
     * column name was changed to upper-case.
     * For example: LLGTG("llgtg")
     */
    public static enum MESOTable
    {
        STRMID("strmID"), IDENT("ident"), AZM("azm"), RNG("rng"), MDASR("mdaSR"), CLASS("class"),
        LLVR("llVr"), LLGTG("llgtg"), BASE("base"), DEPTH("depth"), RELDEP("relDep"), MAXVR("maxVr"),
        HTMXVR("htMxVr"), TVS("tvs"), DIR("dir"), SPD("spd"), MSI("msi"), LAT("lat"),
        LON("lon"), COUNTY("county");
        
        private String colName;
        
        MESOTable(String name)
        {
            colName = name;
        }
        
        public String getColName()
        {
            return colName;
        }
    };
    
    /**
     * TVS table enumeration.  Each identifier MUST match the column name in quotes if the
     * column name was changed to upper-case.
     * For example: MXDVHT("mxDvHt")
     */
    public static enum TVSTable
    {
        STRMID("strmID"), IDENT("ident"), TYPE("type"), AZM("azm"), RNG("rng"),
        AVGDV("avgDv"), LLDV("llDv"), MAXDV("maxDv"), MXDVHT("mxDvHt"), BASE("base"),
        DEPTH("depth"), TOP("top"), SHEAR("shear"), SHRHT("shrHt"), LAT("lat"),
        LON("lon"), COUNTY("county");
        
        private String colName;
        
        TVSTable(String name)
        {
            colName = name;
        }
        
        public String getColName()
        {
            return colName;
        }
    };
}
