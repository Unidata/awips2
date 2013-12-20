package com.raytheon.uf.common.dataplugin.warning.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

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
 * Apr 28, 2013     1955   jsanchez     Added an ignoreUserData flag to intersection method.
 * Oct 21, 2013 DR 16632   D. Friedman  Handle zero-length input in union.
 * Dec 13, 2013 DR 16567   Qinglu Lin   Added contains(). TEST gerrit!
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
     * Checks to see if g1 contains the point
     * 
     * @param g1
     * @param g2
     * @return
     */
    public static boolean contains(Geometry g1, Point p) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                if (contains(g1.getGeometryN(i), p)) {
                    return true;
                }
            }
            return false;
        } else {
            return g1.contains(p);
        }
    }

    /**
     * Intersection between g1 and g2. Resulting intersection will contain user
     * data from g2
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
        intersection(g1, g2, intersection, false);
        Geometry rval = gf.createGeometryCollection(intersection
                .toArray(new Geometry[intersection.size()]));
        rval.setUserData(g2.getUserData());
        return rval;
    }

    private static void intersection(Geometry g1, Geometry g2,
            List<Geometry> intersections, boolean ignoreUserData) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                intersection(g1.getGeometryN(i), g2, intersections,
                        ignoreUserData);
            }
        } else {
            if (g2 instanceof GeometryCollection) {
                for (int i = 0; i < g2.getNumGeometries(); ++i) {
                    intersection(g1, g2.getGeometryN(i), intersections,
                            ignoreUserData);
                }
            } else {
                String g1Name = toString(g1.getUserData());
                String g2Name = toString(g2.getUserData());

                if (g1Name == null || g2Name == null || g2Name.equals(g1Name)
                        || ignoreUserData) {
                    Geometry section = g1.intersection(g2);
                    if (section.isEmpty() == false) {
                        if (g2.getUserData() != null) {
                            if (section instanceof GeometryCollection) {
                                for (int n = 0; n < section.getNumGeometries(); ++n) {
                                    setUserData(section.getGeometryN(n),
                                            (CountyUserData) g2.getUserData());
                                }
                            } else {
                                setUserData(section,
                                        (CountyUserData) g2.getUserData());
                            }
                        }
                        intersections.add(section);
                    }
                }
            }
        }
    }

    /**
     * Intersection between g1 and prepared geometry pg. Resulting Geometry will
     * have user data from pg. Using this method assumes that g1 and pg come
     * from the same area source (i.e County, Zone)
     * 
     * @param g1
     * @param g2
     * 
     * @return the intersection between g1 and g2
     */
    public static Geometry intersection(Geometry g1, PreparedGeometry pg) {
        return intersection(g1, pg, false);
    }

    /**
     * Intersection between g1 and prepared geometry pg. Resulting Geometry will
     * have user data from pg. Setting ignoreUserDate to 'true' will collect
     * intersecting geometries although g1 and pg are from different sources.
     * 
     * @param g1
     * @param pg
     * @param ignoreUserData
     * @return
     */
    public static Geometry intersection(Geometry g1, PreparedGeometry pg,
            boolean ignoreUserData) {
        GeometryFactory gf = new GeometryFactory();
        List<Geometry> intersection = new ArrayList<Geometry>(
                g1.getNumGeometries() + 1);
        intersection(g1, pg, intersection, ignoreUserData);
        Geometry rval = gf.createGeometryCollection(intersection
                .toArray(new Geometry[intersection.size()]));
        rval.setUserData(pg.getGeometry().getUserData());
        return rval;
    }

    private static void intersection(Geometry g1, PreparedGeometry pg,
            List<Geometry> intersections, boolean ignoreUserData) {
        if (g1 instanceof GeometryCollection) {
            for (int i = 0; i < g1.getNumGeometries(); ++i) {
                intersection(g1.getGeometryN(i), pg, intersections,
                        ignoreUserData);
            }
        } else {
            String g1Name = toString(g1.getUserData());
            String g2Name = toString(pg.getGeometry().getUserData());

            if ((g2Name != null && g2Name.equals(g1Name))
                    || ((g1Name == null || g2Name == null || ignoreUserData) && pg
                            .intersects(g1))) {
                Geometry g2 = pg.getGeometry();
                intersection(g1, g2, intersections, ignoreUserData);
            }
        }
    }

    /**
     * Creates a geometry collection from geoms
     * 
     * @param geoms
     * @return
     */
    public static Geometry union(Geometry... geoms) {
        List<Geometry> geometries = new ArrayList<Geometry>(
                geoms.length > 0 ? geoms[0].getNumGeometries() + 1 : 0);
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
    
    /**
     * Determine if one geometry contains any portion of the other.
     * @param g1
     * @param g2
     */
    public static boolean contains(Geometry g1, Geometry g2) {
        int m = g1.getNumGeometries();
        int n = g2.getNumGeometries();
        Geometry geom1 = null;
        Geometry geom2 = null;
        for (int i = 0; i < m; i++) {
            geom1 = g1.getGeometryN(i);
            for (int j = 0; j < n; j++) {
                geom2 = g2.getGeometryN(j);
                if (geom1.contains(geom2))
                    return true;
            }
        }
        return false;
    }    
}
