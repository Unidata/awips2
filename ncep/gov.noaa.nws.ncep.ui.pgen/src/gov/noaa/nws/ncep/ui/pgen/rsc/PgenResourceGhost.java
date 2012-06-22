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
package gov.noaa.nws.ncep.ui.pgen.rsc;

import gov.noaa.nws.ncep.ui.pgen.display.AbstractElementContainer;
import gov.noaa.nws.ncep.ui.pgen.display.DefaultElementContainer;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * Ghost drawing for the pgen resource.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2012            bgonzale     Initial creation
 *
 * </pre>
 *
 * @author bgonzale
 * @version 1.0	
 */

public class PgenResourceGhost {
    public AbstractDrawableComponent component;
    Map<Object, AbstractElementContainer> componentMap = new HashMap<Object, AbstractElementContainer>();
    
    /**
     * Draw the ghost
     * @param target
     * @param paintProps
     * @param df
     * @param descriptor
     */
    public void draw( IGraphicsTarget target, PaintProperties paintProps,
            DisplayElementFactory df, IMapDescriptor descriptor){
        df.setLayerDisplayAttr( false, null, false );
        if (component != null) {
            Iterator<DrawableElement> iterator = component
                    .createDEIterator();
            int count = 0;
            while (iterator.hasNext()) {
                DrawableElement element = iterator.next();
                drawElement(target, paintProps, df, element, descriptor);
                ++count;
            }
        }
    }
    
    /**
     * Creates displayables for an element using an ElementContainer and call the 
     * displayables' draw() method to draw the element.
     * @param target        Graphic target
     * @param paintProps    Paint properties
     * @param df            Display element factory
     * @param el            Input drawable element
     * @praram descriptor
     */
    private void drawElement( IGraphicsTarget target, PaintProperties paintProps,
            DisplayElementFactory df, DrawableElement el, IMapDescriptor descriptor){
        Object key = createKey(el);
        AbstractElementContainer graphic =  componentMap.get(key);
        
        if (graphic == null) {
            graphic = new DefaultElementContainer(el, descriptor, target);
            componentMap.put(key, graphic);
        } else {
            graphic.setElement(el);
        }
        graphic.draw(target, paintProps, null, true);
    }
            
    private Object createKey(DrawableElement el) {
        return el.getPgenCategory()+ ":"+el.getPgenType();
    }

    /**
     * Sets the ghost line for the PGEN drawing layer.
     * @param ghost 
     */
    public void setGhostLine(AbstractDrawableComponent ghost) {
		this.component = ghost;
    }
}
