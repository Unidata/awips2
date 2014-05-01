/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Function object for temporal filter functions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class TimeFunction extends FilterFunction {

    public static class TimeStampAdd extends TimeFunction {

        protected String fieldName;

        protected String units;

        protected int interval;

        /**
         * @param args
         * @throws Exception
         */
        public TimeStampAdd(String[] args) throws Exception {
            super(args);
            if (args.length != 3) {
                throw new Exception("Invalid argument list: " + args);
            }
            fieldName = args[0];
            units = checkUnit(args[1]);
            interval = checkInt(args[2]);

            // TODO set literal based on fieldName
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.edex.wfs.filter.v2_0_0.FilterFunction#toSql()
         */
        @Override
        public String toSql() {
            return String.format("{%s} + interval '%d %s'", fieldName,
                    interval, units);
        }
    }

    public static class TimeStampDiff extends TimeFunction {

        protected String field0;

        protected String field1;

        protected String units;

        private static Map<String, Integer> intMap = new HashMap<String, Integer>();

        static {
            // since we don't know specifics, we use average year and month
            intMap.put("YEAR", 31557600);
            intMap.put("MONTH", 31557600 / 12);
            intMap.put("DAY", 86400);
            intMap.put("HOUR", 3600);
            intMap.put("MINUTE", 60);
            intMap.put("SECOND", 1);
        }

        /**
         * @param args
         * @throws Exception
         */
        public TimeStampDiff(String[] args) throws Exception {
            super(args);
            if (args.length != 3) {
                throw new Exception("Invalid argument list: " + args);
            }
            field0 = args[0];
            field1 = args[1];
            units = checkUnit(args[2]);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.edex.wfs.filter.v2_0_0.FilterFunction#toSql()
         */
        @Override
        public String toSql() {
            int seconds = intMap.get(units.toUpperCase());
            return String.format("extract(epoch from {%s} - {%s})/%d", field0,
                    field1, seconds);
        }
    }

    protected final String[] args;

    protected static final String[] UNITS = { "YEAR", "MONTH", "DAY", "HOUR",
            "MINUTE", "SECOND" };

    protected static final Set<String> UNIT_SET = new HashSet<String>(
            Arrays.asList(UNITS));

    public TimeFunction(String[] args) {
        this.args = args;
    }

    protected String checkUnit(String units) throws Exception {
        if (!UNIT_SET.contains(units.toUpperCase())) {
            throw new Exception("Unsupported time function units: " + units);
        }
        return units;
    }

    protected int checkInt(String intVal) throws Exception {
        try {
            return Integer.parseInt(intVal);
        } catch (Throwable t) {
            throw new Exception("Invalid integer argument to function: "
                    + intVal);
        }
    }

    /**
     * @param name
     * @param args
     * @return null if name doesn't match any supported functions
     * @throws Exception
     */
    public static TimeFunction create(String name, String[] args)
            throws Exception {
        if ("TIMESTAMPADD".equalsIgnoreCase(name)) {
            return new TimeStampAdd(args);
        } else if ("TIMESTAMPDIFF".equalsIgnoreCase(name)) {
            return new TimeStampDiff(args);
        }
        return null;
    }

}
