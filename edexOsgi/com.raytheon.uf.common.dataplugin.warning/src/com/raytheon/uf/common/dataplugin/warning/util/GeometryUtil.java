package com.raytheon.uf.common.dataplugin.warning.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.operation.overlay.snap.GeometrySnapper;

/**
 * 
 * Performs common geometry operations taking geometry collections into
 * account for counties. Makes certain assumptions about these geometries that
 * only apply to warngen
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class GeometryUtil {

    private static final String SEPARATOR = "_";

    public static boolean equals(Geometry g1, Geometry g2) {
        if (g1 == null && g2 == null) {
            return true;
        } else if (g1 == null) {
            return false;
        } else if (g2 == null) {
            return false;
        }
        List<Geometry> list1 = new ArrayList<Geometry>();
        List<Geometry> list2 = new ArrayList<Geometry>();
        buildGeometryList(list1, g1);
        buildGeometryList(list2, g2);

        if (list1.size() != list2.size()) {
            return false;
        }

        for (int i = 0; i < list1.size(); ++i) {
            if (list1.get(i).equals(list2.get(i)) == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param parentGeom
     * @param geometry
     * @return
     */
    public static boolean intersects(Geometry g1, Geometry g2) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                if (intersects(g1.getGeometryN(i), g2)) {
                    return true;
                }
            }
            return false;
        } else if (g2 instanceof GeometryCollection) {
            for (int i = 0; i < g2.getNumGeometries(); ++i) {
                if (intersects(g1, g2.getGeometryN(i))) {
                    return true;
                }
            }
            return false;
        } else {
            return g1.intersects(g2);
        }
    }

    /**
     * Checks to see if g1 contains g2
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static boolean contains(Geometry g1, Geometry g2) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                if (contains(g1.getGeometryN(i), g2)) {
                    return true;
                }
            }
            return false;
        } else {
            return g1.contains(g2);
        }
    }

    /**
     * Checks to see if g intersects any pGs
     * 
     * @param pGs
     * @param g
     * @return
     */
    public static boolean intersects(List<PreparedGeometry> pGs, Geometry g) {
        for (PreparedGeometry pg : pGs) {
            if (pg.intersects(g)) {
                return true;
            }
        }
        return false;
    }

    public static void recursivePreparedGeometry(Geometry g,
            List<PreparedGeometry> prepGeom) {
        if (g instanceof GeometryCollection) {
            for (int i = 0; i < g.getNumGeometries(); ++i) {
                recursivePreparedGeometry(g.getGeometryN(i), prepGeom);
            }
        } else {
            prepGeom.add(PreparedGeometryFactory.prepare(g));
        }
    }

    /**
     * Intersection between g1 and g2
     * 
     * @param g1
     * @param g2
     * 
     * @return the intersection between g1 and g2
     */
    public static Geometry intersection(Geometry g1, Geometry g2) {
        GeometryFactory gf = new GeometryFactory();
        List<Geometry> intersection = new ArrayList<Geometry>(
                g1.getNumGeometries() + g2.getNumGeometries());
        intersection(g1, g2, intersection);
        Geometry rval = gf.createGeometryCollection(intersection
                .toArray(new Geometry[intersection.size()]));
        for (int i = 0; i < rval.getNumGeometries(); ++i) {
            setUserData(rval.getGeometryN(i), (CountyUserData) intersection
                    .get(i).getUserData());
            // why set it again??
            rval.getGeometryN(i).setUserData(intersection.get(i).getUserData());
        }
        rval.setUserData(g2.getUserData());
        return rval;
    }

    /**
     * Intersection between g1 and g2
     * 
     * @param g1
     * @param g2
     * 
     * @return the intersection between g1 and g2
     */
    public static Geometry intersection(Geometry g1, PreparedGeometry pg) {
        GeometryFactory gf = new GeometryFactory();
        List<Geometry> intersection = new ArrayList<Geometry>(
                g1.getNumGeometries() + 1);
        intersection(g1, pg, intersection);
        Geometry rval = gf.createGeometryCollection(intersection
                .toArray(new Geometry[intersection.size()]));
        for (int i = 0; i < rval.getNumGeometries(); ++i) {
            setUserData(rval.getGeometryN(i), (CountyUserData) intersection
                    .get(i).getUserData());
            // why set it again??
            rval.getGeometryN(i).setUserData(intersection.get(i).getUserData());
        }
        rval.setUserData(pg.getGeometry().getUserData());
        return rval;
    }

    private static void intersection(Geometry g1, Geometry g2,
            List<Geometry> intersections) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                intersection(g1.getGeometryN(i), g2, intersections);
            }
        } else {
            if (g2 instanceof GeometryCollection) {
                for (int i = 0; i < g2.getNumGeometries(); ++i) {
                    intersection(g1, g2.getGeometryN(i), intersections);
                }
            } else {
                String g1Name = toString(g1.getUserData());
                String g2Name = toString(g2.getUserData());
                String prefix = null;
                if (g1Name != null && g2Name != null) {
                    prefix = getPrefix(g1Name);
                }
                if ((g1Name == null || g2Name == null || g2Name
                        .startsWith(prefix))) {
                    if (g1.isValid() && g2.isValid()) {
                        Geometry section = g1.intersection(g2);
                        if (section.isEmpty() == false) {
                            section = section.buffer(0);
                            setUserData(section,
                                    (CountyUserData) g2.getUserData());
                            section.setUserData(g2.getUserData());
                            intersections.add(section);
                        }
                    }
                }
            }
        }
    }

    private static void intersection(Geometry g1, PreparedGeometry pg,
            List<Geometry> intersections) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                intersection(g1.getGeometryN(i), pg, intersections);
            }
        } else {
            if (pg.intersects(g1)) {
                Geometry g2 = pg.getGeometry();
                List<Geometry> sections = new ArrayList<Geometry>();
                for (int n = 0; n < g2.getNumGeometries(); n++) {
                    Geometry section = g1.intersection(g2.getGeometryN(n));
                    if (section.isEmpty() == false) {
                        sections.add(section);
                    }
                }
                Geometry section = null;
                if (sections.size() == 1) {
                    section = sections.get(0);
                } else if (sections.size() > 0) {
                    section = new GeometryFactory()
                            .createGeometryCollection(sections
                                    .toArray(new Geometry[0]));
                }
                if (section != null && section.isEmpty() == false) {
                    setUserData(section, (CountyUserData) g2.getUserData());
                    intersections.add(section);
                }
            }
        }
    }

    /**
     * Get the difference between the 2 geometries
     * 
     * @param g1
     *            main geometry
     * @param g2
     *            geometry to remove
     * @return g1 minus g2
     */
    public static Geometry difference(Geometry g1, Geometry g2) {
        GeometryFactory gf = new GeometryFactory();
        List<Geometry> differences = new ArrayList<Geometry>();
        buildGeometryList(differences, g1);
        Set<String> prefixes = new HashSet<String>();

        String pre = getPrefix(toString(g2.getUserData()));
        if (pre != null) {
            // A one county geometry
            prefixes.add(pre);
        } else {
            // Multi county geometry, add each unique prefix
            for (int j = 0; j < g2.getNumGeometries(); ++j) {
                Geometry g2g = g2.getGeometryN(j);
                prefixes.add(getPrefix(toString(g2g.getUserData())));
            }
        }

        for (String prefix : prefixes) {
            // For each prefix, remove any geometries from g1 that have the same
            // prefix
            if (prefix != null) {
                List<Geometry> toRemove = new ArrayList<Geometry>();
                for (int i = 0; i < g1.getNumGeometries(); ++i) {
                    Geometry g1g = g1.getGeometryN(i);
                    String g1gPrefix = getPrefix(toString(g1g.getUserData()));
                    if (g1gPrefix.equals(prefix)) {
                        toRemove.add(g1g);
                    }
                }
                differences.removeAll(toRemove);
            }
        }

        return gf.createGeometryCollection(differences
                .toArray(new Geometry[differences.size()]));
    }

    /**
     * Creates a geometry collection from geoms
     * 
     * @param geoms
     * @return
     */
    public static Geometry union(Geometry... geoms) {
        List<Geometry> geometries = new ArrayList<Geometry>(
                geoms[0].getNumGeometries() + 1);
        for (Geometry g : geoms) {
            buildGeometryList(geometries, g);
        }
        return new GeometryFactory().createGeometryCollection(geometries
                .toArray(new Geometry[geometries.size()]));
    }

    public static void buildGeometryList(List<Geometry> geometryParts,
            Geometry geometry) {
        for (int i = 0; i < geometry.getNumGeometries(); ++i) {
            Geometry g = geometry.getGeometryN(i);
            if (g instanceof Polygon && g.isEmpty() == false) {
                geometryParts.add(g);
            } else if (g instanceof GeometryCollection) {
                buildGeometryList(geometryParts, g);
            }
        }
    }

    public static Geometry selfSnap(Geometry g, double snapTolerance) {
        GeometrySnapper snapper = new GeometrySnapper(g);
        Geometry snapped = snapper.snapTo(g, snapTolerance);
        // need to "clean" snapped geometry - use buffer(0) as a simple way to
        // do this
        Geometry fix = snapped.buffer(0);
        return fix;
    }

    public static void printGeoemtry(Geometry g) {
        printGeoemtry(g, 0);
    }

    private static void printGeoemtry(Geometry g, int idx) {
        for (int i = 0; i < idx; ++i) {
            System.out.print("  ");
        }
        System.out.println(g.getUserData() + " " + g.getGeometryType() + ", "
                + g.getNumPoints() + " points");
        if (g instanceof GeometryCollection) {
            for (int i = 0; i < g.getNumGeometries(); ++i) {
                printGeoemtry(g.getGeometryN(i), idx + 1);
            }
        }
    }

    public static String[] getGID(Geometry g) {
        List<Geometry> geoms = new ArrayList<Geometry>();
        buildGeometryList(geoms, g);
        Set<String> set = new TreeSet<String>();
        for (Geometry geom : geoms) {
            if (geom.getUserData() != null) {
                set.add(getPrefix(geom.getUserData()));
            }
        }
        return set.toArray(new String[set.size()]);
    }

    /**
     * Recursively set the name of the geometry in the userData
     * 
     * @param g
     * @param prefix
     * @param suffix
     */
    public static void setUserData(Geometry g, CountyUserData data) {
        setUserData(g, data, "");
    }

    /**
     * @param g
     * @param name
     * @param string
     */
    private static void setUserData(Geometry g, CountyUserData data,
            String suffix) {
        String prefix = data.gid;
        CountyUserData newData = new CountyUserData(data.entry, prefix + suffix);
        g.setUserData(newData);
        if (g instanceof GeometryCollection) {
            for (int i = 0; i < g.getNumGeometries(); ++i) {
                setUserData(g.getGeometryN(i), data, suffix + SEPARATOR + i);
            }
        }
    }

    public static String getPrefix(Object userData) {
        String id = String.valueOf(userData);
        int idx = id.indexOf(SEPARATOR);
        String prefix = id;
        if (idx > -1) {
            prefix = id.substring(0, idx);
        }
        return prefix;
    }

    private static String toString(Object userData) {
        String str = null;
        if (userData != null) {
            str = userData.toString();
        }
        return str;
    }
}
