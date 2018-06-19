package com.raytheon.uf.edex.activetable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.ActiveTableUtil;
import com.raytheon.uf.common.activetable.DumpActiveTableRequest;
import com.raytheon.uf.common.activetable.DumpActiveTableResponse;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.site.SiteMap;

public class DumpActiveTableHandler implements
        IRequestHandler<DumpActiveTableRequest> {

    /**
     * Comparator class to sort active table records by issueTime, or by
     * phenomenon for records with the same issue time.
     * 
     */
    class IssueTimeComp implements Comparator<Map<String, Object>> {
        @Override
        public int compare(Map<String, Object> o1, Map<String, Object> o2) {
            int rtnVal;
            // sometimes issueTime arrives as a Long and sometimes as an
            // Integer, possibly depending on whether it is coming from EDEX or
            // a gzipped archive. Convert the values to longs and compare them.
            long issT1 = ((Number) o1.get("issueTime")).longValue();
            long issT2 = ((Number) o2.get("issueTime")).longValue();
            rtnVal = (issT1 < issT2) ? -1 : ((issT1 > issT2) ? 1 : 0);
            if (rtnVal == 0) {
                rtnVal = ((String) o1.get("phen")).compareTo((String) o2
                        .get("phen"));
            }
            return rtnVal;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public DumpActiveTableResponse handleRequest(DumpActiveTableRequest request)
            throws Exception {
        DumpActiveTableResponse response = new DumpActiveTableResponse();
        List<Map<String, Object>> table = null;

        String[] orgSites = request.getSites();
        String[] sites = null;
        if (orgSites != null) {
            sites = new String[orgSites.length];
            System.arraycopy(orgSites, 0, sites, 0, orgSites.length);
        }
        request.setSites(sites);
        try {

            if ("FILE".equals(request.getMode())) {

                table = fileStringToActiveTable(request.getFileName(),
                        request.getFileContent());
                convertToFourChar(sites);

            } else {

                String fromSite = request.getFromSite();
                if (fromSite == null || fromSite.length() == 0) {
                    if (sites == null || sites.length == 0) {
                        response.setMessage("Specify at least one site or set the GFESUITE_SITEID environment variable.");
                        response.setDump("");
                        return response;
                    } else {
                        fromSite = sites[0];
                    }
                }

                if (fromSite.length() == 3) {
                    fromSite = SiteMap.getInstance().getSite4LetterId(fromSite);
                }
                convertToFourChar(sites);

                List<ActiveTableRecord> tableOfRecords = getActiveTableForSites(
                        sites, fromSite, request.getMode());
                table = ActiveTableUtil.convertToDict(tableOfRecords, fromSite);
            }

            response.setUnfilteredCount(Integer.valueOf(table.size()));
            filter(table, request);
            response.setFilteredCount(Integer.valueOf(table.size()));

            Collections.sort(table, new IssueTimeComp());

            // Start setting up the Python script to print the table
            String fileName = "vtec/VTECTableUtil.py";
            IPathManager pathManager = PathManagerFactory.getPathManager();
            String scriptPath = pathManager.getStaticFile(fileName)
                    .getAbsolutePath();

            // VTECTableUtil imports LogStream, though it doesn't use it.
            String includePath = pathManager
                    .getStaticFile("python/LogStream.py").getParentFile()
                    .getAbsolutePath();

            List<String> preEvals = new ArrayList<String>();

            PythonScript python = null;

            // parameters for VTECTableUtil ctor
            Map<String, Object> initArgs = new HashMap<String, Object>();

            // parameters for printActiveTable call
            Map<String, Object> args = new HashMap<String, Object>();

            args.put("table", table);
            try {
                python = new PythonScript(scriptPath, includePath, this
                        .getClass().getClassLoader(), preEvals);
                python.instantiatePythonClass("vtecTableUtil", "VTECTableUtil",
                        initArgs);
                String dump = (String) python.execute("printActiveTable",
                        "vtecTableUtil", args);
                response.setDump(dump);
            } catch (JepException e) {
                throw new Exception("JepException caught.", e);
            } finally {
                if (python != null) {
                    python.dispose();
                }
            }
            return response;
        } finally {
            request.setSites(orgSites);
        }
    }

    /**
     * Convert an array of sites to the 4-char equivalent sites, in-place.
     * 
     * @param sites
     */
    protected void convertToFourChar(String[] sites) {
        // Convert sites to the 4-char equivalent
        if (sites != null) {
            String site = null;
            for (int idx = 0; idx < sites.length; idx++) {
                site = sites[idx];
                if (site.length() == 3) {
                    sites[idx] = SiteMap.getInstance().getSite4LetterId(site);
                }
            }
        }
    }

    /**
     * Get the backup dict from a VTECPartners script. This is a method so that
     * warnings can be suppressed on the required cast.
     * 
     * @param partners
     * @return
     */
    @SuppressWarnings("unchecked")
    protected Map<String, Object> getBackupDict(VTECPartners partners) {
        Map<String, Object> backupDict = (Map<String, Object>) partners
                .getattr("BackupDict");
        return backupDict;
    }

    /**
     * @param sites
     * @param mode
     * @return
     */
    protected List<ActiveTableRecord> getActiveTableForSites(String[] sites,
            String fromSite, String mode) {
        ActiveTableMode atMode = ActiveTableMode.valueOf(mode);

        return ActiveTable.getActiveTable(fromSite, atMode, null, null, null,
                sites);
    }

    /**
     * @param request
     * @return
     */
    protected void filter(List<Map<String, Object>> records,
            DumpActiveTableRequest request) {

        // If there are no initial records, skip all the filtering code.
        if (records != null && records.size() > 0) {

            // Get the various filter value arrays
            String[] sites = request.getSites();
            String[] phens = request.getPhens();
            String[] sigs = request.getSigs();
            String[] actions = request.getActions();
            String[] pils = request.getPils();
            String[] ids = request.getIds();
            String[] etns = request.getEtns();

            // Sort the arrays so we can filter with BinarySearch
            if (sites != null) {
                Arrays.sort(sites);
            }
            if (phens != null) {
                Arrays.sort(phens);
            }
            if (sigs != null) {
                Arrays.sort(sigs);
            }
            if (actions != null) {
                Arrays.sort(actions);
            }
            if (pils != null) {
                Arrays.sort(pils);
            }
            if (ids != null) {
                Arrays.sort(ids);
            }

            // Filter records. Use an iterator instead of a for() loop
            // so we can use itr.remove() to eliminate failed records.
            Iterator<Map<String, Object>> itr = records.iterator();
            while (itr.hasNext()) {
                Map<String, Object> record = itr.next();
                if (rejectedByFilter(sites, (String) record.get("officeid"))
                        || rejectedByFilter(phens, (String) record.get("phen"))
                        || rejectedByFilter(sigs, (String) record.get("sig"))
                        || rejectedByFilter(actions, (String) record.get("act"))
                        || rejectedByFilter(pils, (String) record.get("pil"))
                        || rejectedByFilter(etns, record.get("etn").toString())
                        || rejectedByFilter(ids, (String) record.get("id"))) {
                    itr.remove();
                }
            }
        }
    }

    /**
     * Compare a value to a sorted array of acceptable values. If the array is
     * null or empty, the value is not rejected. Otherwise, the value is
     * rejected if it is not in the array.
     * 
     * @param sortedFilterVals
     *            An array of values to accept. An empty or null array indicates
     *            that no values are to be rejected.
     * @param val
     *            The value to test.
     * @return True if the value is rejected, false otherwise.
     */
    protected boolean rejectedByFilter(String[] sortedFilterVals, String val) {
        boolean rtnval = false;
        if (sortedFilterVals != null && sortedFilterVals.length > 0
                && Arrays.binarySearch(sortedFilterVals, val) < 0) {
            rtnval = true;
        }
        return rtnval;
    }

    /**
     * Users can pretty-print active table backups by invoking dumpAT with the
     * -f &lt;fileName&gt; option. The file is an active table backup: a Python
     * list of dicts, serialized with Pickle or cPickle, usually compressed with
     * gzip. This method invokes a Python script to convert the pickled content
     * to the Java List of Map format, so that it can be processed just like the
     * current operational active table.
     * 
     * @param fileName
     *            The name of the file. This is mostly used to recognize gzipped
     *            files.
     * @param fileContent
     *            The actual content of the file.
     * @return
     */
    @SuppressWarnings("unchecked")
    protected List<Map<String, Object>> fileStringToActiveTable(
            String fileName, String fileContent) {

        // Start setting up the Python script to read the table
        String scriptName = "vtec/BackupConverter.py";
        IPathManager pathManager = PathManagerFactory.getPathManager();
        String scriptPath = pathManager.getStaticFile(scriptName)
                .getAbsolutePath();

        //
        String includePath = pathManager.getStaticFile("python/JUtil.py")
                .getParentFile().getAbsolutePath();

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("fileName", fileName);
        args.put("fileContent", fileContent);
        List<Map<String, Object>> rtnVal = null;

        PythonScript python = null;
        try {
            // The file has a pickled active table, possibly gzipped.
            // unpickle it and convert it to Java.
            python = new PythonScript(scriptPath, includePath, getClass()
                    .getClassLoader());
            python.instantiatePythonClass("converter", "BackupConverter", null);
            rtnVal = (List<Map<String, Object>>) python.execute(
                    "convertToJava", "converter", args);
        } catch (JepException e) {
            throw new RuntimeException("JepException caught", e);
        } finally {
            if (python != null) {
                python.dispose();
            }
        }
        return rtnVal;
    }
}
