package gov.noaa.nws.ncep.viz.gempak.grid.inv;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcDataTree;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcEventNode;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcLevelNode;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcParameterNode;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcSourceNode;
/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    		Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2011            gamazaychikov    Initial creation
 * Nov 17, 2011            Xguo             Fixed getRequestContrainsMap problem
 * </pre>
 * 
 * @author gamazaychikov
 * @version 1.0
 */

public class NcInventory {
	protected class StackEntry {

        public StackEntry(String source, String event, String parameter, long level) {
            super();
            this.source = source;
            this.parameter = parameter;
            this.level = level;
            this.event = event;
        }

        final public String source;
        
        final public String event;

        final public String parameter;

        final public long level;

        public boolean recursive = false;

        public boolean autoAverage = false;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + (int) (level ^ (level >>> 32));
            result = prime * result
                    + ((source == null) ? 0 : source.hashCode());
            result = prime * result
            + ((event == null) ? 0 : event.hashCode());
            result = prime * result
                    + ((parameter == null) ? 0 : parameter.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            StackEntry other = (StackEntry) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (level != other.level)
                return false;
            if (source == null) {
                if (other.source != null)
                    return false;
            } else if (!source.equals(other.source))
                return false;
            if (event == null) {
                if (other.event != null)
                    return false;
            } else if (!event.equals(other.event))
                return false;
            if (parameter == null) {
                if (other.parameter != null)
                    return false;
            } else if (!parameter.equals(other.parameter))
                return false;
            return true;
        }

        private NcInventory getOuterType() {
            return NcInventory.this;
        }

    };
	private boolean isInventoryInited = false;
	
	protected NcDataTree ncDataTree;

	public NcDataTree getNcDataTree() {
		return ncDataTree;
	}

	public void setNcDataTree(NcDataTree ncDataTree) {
		this.ncDataTree = ncDataTree;
	}

	private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NcInventory.class);

    private static NcInventory instance = null;
	public static NcInventory getInstance() {
		if (instance == null) {
			instance = new NcInventory();	
		}
		return instance;
	}
	
	public void initInventory() {
		if ( !isInventoryInited) {
			initTree();
			isInventoryInited = true;
		}
	}


    public void initTree() {
    	ncDataTree = createBaseTree();
    }
    
    protected NcDataTree createBaseTree() {
    	NcDataTree newTree = getTreeFromEdex();
    	
    	if (newTree == null) {
            return newTree;
        }
        return newTree;
	}

    private NcDataTree getTreeFromEdex() {
        String request = "from gov.noaa.nws.ncep.edex.uengine.tasks.ncgrib import NcgridCatalog\n"
                + "from com.raytheon.uf.common.message.response import ResponseMessageGeneric\n"
                + "test = NcgridCatalog()\n"
                + "return ResponseMessageGeneric(test.execute())";
        Object[] tree = null;
        try {
            tree = Connector.getInstance().connect(request, null, 60000);
        } catch (VizCommunicationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error communicating with server.", e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred while retrieving grid tree.", e);
        }
        if (tree != null) {
            return (NcDataTree) tree[0];
        }
        return null;
    }

	public void reinitTree() {
        initTree();
    }
	
	public Map<String, RequestConstraint> getRequestConstraintMap (String source, String event, String parameter, String levelName, String levelValue) {
		
		boolean continueSearch = true;
		Set<String> sources = ncDataTree.getNcSources();
		Iterator<String> sit = sources.iterator();
		while ( sit.hasNext() && continueSearch ) {
			NcSourceNode snode = ncDataTree.getNcSourceNode(source);
			if ( snode == null ) {
				continueSearch = false;
				return null;
			}
			if ( !snode.containsChildNode(event) ) {
				continueSearch = false;
				return null;
			}
			NcEventNode enode = snode.getChildNode(event);
			if ( enode == null ) {
				continueSearch = false;
				return null;
			}
			if ( !enode.containsChildNode(parameter) ) {
				continueSearch = false;
				return null;
			}
			NcParameterNode pnode = enode.getChildNode(parameter);
			if ( pnode == null ) {
				continueSearch = false;
				return null;
			}
			
			String levelId = levelName + ":" + levelValue;
			NcLevelNode lnode = pnode.getChildNode(levelId);
			//if ( lnode.getLevelName().equalsIgnoreCase(levelName) ) {
			if ( lnode != null ) {	
				return lnode.getRcmap();
			}
			else {
				continueSearch = false;
			}
			return null;
		}
        return null;
    }

	public Map<String, RequestConstraint> getRequestConstraintMap(
			String source, String parameter, String levelName, String levelValue) {

		if ( ncDataTree == null ) {
			return null;
		}
		NcSourceNode snode = ncDataTree.getNcSourceNode(source);
		if ( snode == null ) {
			return null;
		}
		for (NcEventNode enode : snode.getChildNodes().values()) {
			if ( enode.containsChildNode(parameter) ) {
				NcParameterNode pnode = enode.getChildNode(parameter);
				if ( pnode != null ) {
					String levelId = levelName + ":" + levelValue;
					NcLevelNode lnode = pnode.getChildNode(levelId);
					if ( lnode != null) {
						return lnode.getRcmap();
					}
				}
			}
		}
		return null;
	}
}


