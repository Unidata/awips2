package gov.noaa.nws.ncep.viz.rsc.plotdata.pluginplotproperties;

import java.util.HashMap;
import java.util.Map;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;




public class PluginPlotProperties {

        public final boolean usesPointDataApi;

        /**
         * When this is true all plots will be correlated based on the
         * stationId, otherwise each dataURI is mapped to a specific set of
         * data.
         */
        public final boolean hasDistinctStationId;

        public PluginPlotProperties(boolean usesPointDataApi,
                boolean hasDistinctStationId) {
            this.usesPointDataApi = usesPointDataApi;
            this.hasDistinctStationId = hasDistinctStationId;
        }

        /**
         * This is the goal for all plugins, they should use the new api and
         * they should have distinct stationIds.
         */
        public PluginPlotProperties() {
            this.usesPointDataApi = true;
            this.hasDistinctStationId = true;
        }
        
        private static final Map<String, PluginPlotProperties> pluginProps = new HashMap<String, PluginPlotProperties>();

        static {
            /*
             * These use the original PlotResource, whoever can convert these gets
             * to delete thousands of lines of code, it will be amazing.
             */
//            pluginProps.put("pirep", new PluginPlotProperties(false, false));
//            pluginProps.put("airep", new PluginPlotProperties(false, false));
//            pluginProps.put("acars", new PluginPlotProperties(false, false));


            /*
             * These have a dependency on dataURI because they don't set stationId,
             * In the future if stationId can be set to anything that is even a
             * little unique we can get rid of this
             */
//            pluginProps.put("bufrquikscat", new PluginPlotProperties(true, false));
//            pluginProps.put("radar",        new PluginPlotProperties(true, false));
//            pluginProps.put("lsr",          new PluginPlotProperties(true, false));
//            pluginProps.put("tcg",          new PluginPlotProperties(true, false));
//            pluginProps.put("svrwx",        new PluginPlotProperties(true, false));
//            pluginProps.put("ldadhydro",    new PluginPlotProperties(true, false));
//            pluginProps.put("textPoints",   new PluginPlotProperties(true, false));
            pluginProps.put("ncpirep",      new PluginPlotProperties(true, false));
            pluginProps.put("ncairep",      new PluginPlotProperties(true, false));
            pluginProps.put("nctaf",        new PluginPlotProperties(true, false));
//            pluginProps.put("bufrmosMRF", new PluginPlotProperties(true, false));
            /*
             * The good ones, these don't even need to be here because this is the
             * default behavior, but for now they are included so we have a
             * comprehensive list of which plugins use certain behaviors.
             */
            pluginProps.put("obs", new PluginPlotProperties());
            pluginProps.put("bufrua", new PluginPlotProperties());
            pluginProps.put("sfcobs", new PluginPlotProperties());
            pluginProps.put("modelsounding", new PluginPlotProperties());
            pluginProps.put("bufrmosAVN", new PluginPlotProperties());
            pluginProps.put("bufrmosETA", new PluginPlotProperties());
            pluginProps.put("bufrmosGFS", new PluginPlotProperties());
            pluginProps.put("bufrmosHPC", new PluginPlotProperties());
            pluginProps.put("bufrmosLAMP", new PluginPlotProperties());
            pluginProps.put("bufrmosMRF", new PluginPlotProperties());            
//            pluginProps.put("goessounding", new PluginPlotProperties());
//            pluginProps.put("poessounding", new PluginPlotProperties());
//            pluginProps.put("profiler", new PluginPlotProperties());
//            pluginProps.put("fssobs", new PluginPlotProperties());
//            pluginProps.put("bufrmosNGM", new PluginPlotProperties());
//            pluginProps.put("ldadmesonet", new PluginPlotProperties());
//            pluginProps.put("qc", new PluginPlotProperties());
//            pluginProps.put("bufrascat", new PluginPlotProperties());
//            pluginProps.put("bufrhdw", new PluginPlotProperties());
//            pluginProps.put("bufrmthdw", new PluginPlotProperties());
//            pluginProps.put("bufrssmi", new PluginPlotProperties());
        }        
        
//        public PluginPlotProperties getPluginProperties() {
//            return getPluginProperties(this.metadataMap);
//        }

        public static PluginPlotProperties getPluginProperties(String pluginName) {
            PluginPlotProperties result = pluginProps.get(pluginName);
            if (result == null) {
                result = new PluginPlotProperties();
            }
            return result;
        }

        public static PluginPlotProperties getPluginProperties(Map<String,RequestConstraint> metadataMap){
            RequestConstraint rc = metadataMap.get("pluginName");
            if (rc == null || rc.getConstraintType() != ConstraintType.EQUALS) {
                throw new IllegalArgumentException("Cannot find plugin properties because metadataMap does not specify a plugin.");
            }
            return getPluginProperties(rc.getConstraintValue());
        }        
        

    }