package ext.erdc;

public class BuildingInfo 
{
    private final String info;
    
    private final int   id;     /* Unique Feature (object) Identifier */
    private final float area;   /* spatial areal extent of the footprint */
    private final float avght;  /* mean height of the building feature */
    private final float minht;  /* min height of the building feature */
    private final float maxht;  /* max height of the building feature */
    private final float base;   /* geometric side (i.e. A = 1/2 b * h) */
    private final float orient; /* azimuthl  orientation (degrees) */
    private final float len;    /* longest edge of feature */
    private final float wid;    /* shortest edge of feature*/
    
    public BuildingInfo(int id, float area, float avght, float minht, float maxht,
            float base, float orient, float len, float wid) 
    {
        this.id = id;
        this.area = area;
        this.avght = avght;
        this.minht = minht;
        this.maxht = maxht;
        this.base = base;
        this.orient = orient;
        this.len = len;
        this.wid = wid;

        StringBuffer sb = new StringBuffer(500);
        sb.append(  " Unique identifier \t= "); sb.append(id);
        sb.append(" \n Area (m2)           \t= "); sb.append(area);
        sb.append(" \n Mean height (m)   \t= "); sb.append(avght);
        sb.append(" \n Min height (m)    \t= "); sb.append(minht);
        sb.append(" \n Max height (m)    \t= "); sb.append(maxht);
        sb.append(" \n Base side (m)     \t= "); sb.append(base);
        sb.append(" \n Azimuth (degrees) \t= "); sb.append(orient);
        sb.append(" \n Longest edge (m)  \t= "); sb.append(len);
        sb.append(" \n Shortest edge (m) \t= "); sb.append(wid);sb.append(" ");
        
        info = sb.toString();
    }
    
    public String toString() 
    {
        return info;
    }
}