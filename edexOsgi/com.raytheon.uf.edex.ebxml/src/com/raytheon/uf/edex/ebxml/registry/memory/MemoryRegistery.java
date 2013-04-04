/**
 * 
 */
package com.raytheon.uf.edex.ebxml.registry.memory;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectNotFoundExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ebxml.query.matcher.AndMatcher;
import com.raytheon.uf.edex.ebxml.query.matcher.CompositeMatcher;
import com.raytheon.uf.edex.ebxml.query.matcher.OrMatcher;
import com.raytheon.uf.edex.ebxml.registry.IRegistry;
import com.raytheon.uf.edex.ebxml.registry.memory.matchers.MatcherManager;
import com.raytheon.uf.edex.ebxml.util.EbxmlUtil;

/**
 * Basic implementation of an {@link IRegistry}. Stores data in a map structure
 * and has no persistence.
 * 
 * @author jsherida
 */
public class MemoryRegistery implements IRegistry {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MemoryRegistery.class);

    private static final List<String> SUPPORTED_QUERY_LANGUAGES = new ArrayList<String>();

    static {
        SUPPORTED_QUERY_LANGUAGES.add("XPath");
    }

    private oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();

    private oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

    /** Map<LID, RegistryObject version tree> */
    private final Map<String, RegistryVersionNode> registry;

    /** Map of IDs to LIDs. Tells which LID an ID is in. */
    private final Map<String, String> ids;

    /** Private default constructor. */
    public MemoryRegistery() {
        registry = new HashMap<String, RegistryVersionNode>();
        ids = new HashMap<String, String>();
    }

    /** {@inheritDoc} */
    @Override
    public void init() {
        // Do nothing.
    }

    /** {@inheritDoc} */
    @Override
    public synchronized RegistryExceptionType create(
            List<RegistryObjectType> objs) {
        return putObject(objs);
    }

    /** {@inheritDoc} */
    @Override
    public synchronized RegistryExceptionType replace(
            List<RegistryObjectType> objs) {
        return putObject(objs);
    }

    /** {@inheritDoc} */
    @Override
    public synchronized RegistryExceptionType version(
            List<RegistryObjectType> objs) {
        for (RegistryObjectType obj : objs) {
            String lid = obj.getLid();
            String id = obj.getId();
            RegistryVersionNode node = registry.get(lid);
            if (node == null) {
                registry.put(lid, new RegistryVersionNode(obj));
            } else {
                node.addVersion(obj);
            }
            ids.put(id, lid);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public synchronized RegistryExceptionType remove(List<ObjectRefType> refs) {
        for (ObjectRefType ref : refs) {
            String id = ref.getId();
            String lid = ids.get(id);
            // TODO Remove specific version?
            registry.remove(lid);
            ids.remove(id);
        }

        return null;
    }

    /**
     * Puts the RegistryObject into the registry.
     * 
     * @param objs
     *            the RegistryObject
     */
    private RegistryExceptionType putObject(List<RegistryObjectType> objs) {
        for (RegistryObjectType obj : objs) {
            String lid = obj.getLid();
            String id = obj.getId();
            RegistryVersionNode node = registry.get(lid);
            if (node == null) {
                registry.put(lid, new RegistryVersionNode(obj));
            } else {
                RegistryVersionNode version = node.getObject(id);
                version.setObject(obj);
            }
            ids.put(id, lid);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public synchronized boolean containsId(String id) throws IOException {
        return ids.keySet().contains(id);
    }

    /** {@inheritDoc} */
    @Override
    public synchronized boolean containsLid(String lid) throws IOException {
        return registry.containsKey(lid);
    }

    @Override
    public RegistryExceptionType update(List<ObjectRefType> refs,
            List<UpdateActionType> updateActions, Mode mode) {
        if (mode.equals(Mode.CREATE_ONLY)) {
            InvalidRequestExceptionType e2 = rsFactory
                    .createInvalidRequestExceptionType();
            e2.setMessage("The " + Mode.CREATE_ONLY
                    + " mode is invalid for use with updating objects");
            return e2;
        } else if (mode.equals(!mode.equals(Mode.CREATE_OR_REPLACE)
                && !mode.equals(Mode.CREATE_OR_VERSION))) {
            InvalidRequestExceptionType e3 = rsFactory
                    .createInvalidRequestExceptionType();
            e3.setMessage("Invalid mode specified: " + mode);
            e3.setDetail("Valid modes are: " + Arrays.toString(Mode.values()));
            return e3;
        }

        // Ensure that all of the objects are in the registry.
        for (ObjectRefType ref : refs) {
            try {
                if (!this.containsId(ref.getId())) {
                    ObjectNotFoundExceptionType e = rsFactory
                            .createObjectNotFoundExceptionType();
                    e.setMessage("Object (id=" + ref.getId() + ") not found");
                    return e;
                }
            } catch (IOException ioe) {
                StringWriter writer = new StringWriter();
                ioe.printStackTrace(new PrintWriter(writer));

                RegistryExceptionType e = rsFactory
                        .createRegistryExceptionType();
                e.setMessage("Error contacting registry.");
                e.setDetail(writer.toString());
                e.setCode(ioe.getClass().toString());
                return e;
            }
        }

        for (ObjectRefType ref : refs) {
            if (mode.equals(Mode.CREATE_OR_VERSION)) {
                // Increment the version on this object before updating
                List<RegistryObjectType> objList = new ArrayList<RegistryObjectType>(
                        1);
                objList.add(this.registry.get(ref.getId()).getObject());
                this.version(objList);
            }
            RegistryVersionNode node = this.registry.get(ref.getId())
                    .getLatest();
            RegistryExceptionType e = applyUpdate(node, updateActions);
            if (e != null) {
                return e;
            }
        }
        return null;
    }

    private RegistryExceptionType applyUpdate(RegistryVersionNode node,
            List<UpdateActionType> updateActions) {

        /*
         * Validate the update actions before applying them
         */
        for (UpdateActionType updateAction : updateActions) {
            String updateMode = updateAction.getMode();
            StringQueryExpressionType selector = null;
            if (updateAction.getSelector() instanceof StringQueryExpressionType) {
                selector = (StringQueryExpressionType) updateAction
                        .getSelector();
            } else {
                // TODO: Implement XML QueryType
                QueryExceptionType e = queryFactory.createQueryExceptionType();
                e.setMessage("Unsupported query type specified: "
                        + updateAction.getSelector().getClass().getName());
                e.setDetail("StringQueryType is currently the only supported query type");
                return e;
            }
            String queryLanguage = selector.getQueryLanguage();
            String query = selector.getValue();
            if (!SUPPORTED_QUERY_LANGUAGES.contains(queryLanguage)) {
                QueryExceptionType e = queryFactory.createQueryExceptionType();
                e.setMessage("Unsupported query language specified: "
                        + queryLanguage);
                e.setDetail("Supported language for this registry are: "
                        + SUPPORTED_QUERY_LANGUAGES);
                return e;
            }

            String updateField = query.replaceAll("\\.", "")
                    .replaceAll("/", "");
            // Updating the lid, id, or objectType fields is
            // explicitly not allowed
            if (updateField.equalsIgnoreCase("lid")
                    || updateField.equalsIgnoreCase("id")
                    || updateField.equalsIgnoreCase("objectType")) {
                InvalidRequestExceptionType e = rsFactory
                        .createInvalidRequestExceptionType();
                e.setMessage("The update objects protocol prohibits updating the id, lid, or objectType fields");
                return e;
            }

            if (!EbxmlUtil.isUpdateModeValid(updateMode)) {
                InvalidRequestExceptionType e = rsFactory
                        .createInvalidRequestExceptionType();
                e.setMessage("Invalide update mode specified: " + updateMode);
                e.setDetail("Valid update modes are: " + EbxmlUtil.UPDATE_MODES);
                return e;
            }
        }

        /*
         * This section applies the updates to the specified object after the
         * validation is complete
         */
        for (UpdateActionType updateAction : updateActions) {
            StringQueryExpressionType selector = (StringQueryExpressionType) updateAction
                    .getSelector();
            String updateMode = updateAction.getMode();
            String query = selector.getValue();

        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public List<RegistryObjectType> query(Map<String, List<Object>> parameters,
            boolean matchAny, long startIndex, long maxResults, long depth,
            boolean matchOlderVersions) throws IOException {
        CompositeMatcher matcher;
        if (matchAny) {
            matcher = new OrMatcher();
        } else {
            matcher = new AndMatcher();
        }

        for (String key : parameters.keySet()) {
            List<Object> values = parameters.get(key);
            matcher.add(MatcherManager.getMatcher(key, values));
        }

        List<RegistryObjectType> matchedObjects = new ArrayList<RegistryObjectType>();
        long currentIndex = 0;
        for (String lid : registry.keySet()) {
            List<RegistryVersionNode> nodesToCheck = new ArrayList<RegistryVersionNode>();
            if (matchOlderVersions) {
                nodesToCheck.add(registry.get(lid));
                nodesToCheck.addAll(registry.get(lid).versions);
            } else {
                nodesToCheck.add(registry.get(lid).getLatest());
            }
            for (RegistryVersionNode node : nodesToCheck) {
                if (matcher.matches(node.getObject())) {
                    if (currentIndex >= startIndex
                            && (matchedObjects.size() < maxResults || maxResults == -1)) {
                        matchedObjects.add(node.getObject());
                    }
                    currentIndex++;
                }
            }
        }
        return matchedObjects;
    }

    /**
     * Represents a version node for a {@link RegistryObjectType}.
     * 
     * @author jsherida
     */
    private static class RegistryVersionNode implements Cloneable {
        private String lid;

        private String id;

        private RegistryObjectType obj;

        private Vector<RegistryVersionNode> versions;

        public RegistryVersionNode(RegistryObjectType obj) {
            this.obj = obj;
            this.id = obj.getId();
            this.lid = obj.getLid();
            this.versions = new Vector<RegistryVersionNode>();
        }

        @SuppressWarnings("unused")
        public String getId() {
            return id;
        }

        @SuppressWarnings("unused")
        public String getLid() {
            return lid;
        }

        public void setObject(RegistryObjectType obj) {
            this.obj = obj;
            this.id = obj.getId();
            this.lid = obj.getLid();
            this.versions = new Vector<RegistryVersionNode>();
        }

        public RegistryObjectType getObject() {
            return obj;
        }

        public RegistryVersionNode getLatest() {
            if (versions.size() == 0) {
                return this;
            } else {
                return versions.lastElement().getLatest();
            }
        }

        public RegistryVersionNode getObject(String id) {
            if (this.id.equals(id)) {
                return this;
            } else {
                for (RegistryVersionNode node : versions) {
                    MemoryRegistery.RegistryVersionNode version = node
                            .getObject(id);
                    if (version != null) {
                        return version;
                    }
                }
                return null;
            }
        }

        public void addVersion(RegistryObjectType obj) {
            RegistryVersionNode node = getObject(obj.getLid());
            int idx = obj.getId().lastIndexOf(':');
            if (idx == -1) {
                obj.setId(obj.getId() + ":" + System.currentTimeMillis());
            } else {
                obj.setId(obj.getId().substring(0, idx) + ":"
                        + System.currentTimeMillis());
            }
            node.addChildVersion(obj);
        }

        public void addChildVersion(RegistryObjectType obj) {
            versions.add(new RegistryVersionNode(obj));
        }
    }
}
