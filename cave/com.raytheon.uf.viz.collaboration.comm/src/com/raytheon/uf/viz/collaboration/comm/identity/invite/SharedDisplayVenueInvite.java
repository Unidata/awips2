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
package com.raytheon.uf.viz.collaboration.comm.identity.invite;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class SharedDisplayVenueInvite extends VenueInvite {

    @DynamicSerializeElement
    private UserId sessionLeader;

    @DynamicSerializeElement
    private UserId dataProvider;

    @DynamicSerializeElement
    private ColorPopulator colors;

    public UserId getSessionLeader() {
        return sessionLeader;
    }

    public void setSessionLeader(UserId sessionLeader) {
        this.sessionLeader = sessionLeader;
    }

    public UserId getDataProvider() {
        return dataProvider;
    }

    public void setDataProvider(UserId dataProvider) {
        this.dataProvider = dataProvider;
    }

    /**
     * @return the colors
     */
    public ColorPopulator getColors() {
        return colors;
    }

    /**
     * @param colors
     *            the colors to set
     */
    public void setColors(ColorPopulator colors) {
        this.colors = colors;
    }

    public Map<UserId, RGB> getRGBColors() {
        Map<UserId, RGB> map = new HashMap<UserId, RGB>();
        for (int i = 0; i < colors.getUserName().size(); i++) {
            RGB rgb = new RGB(colors.getRed().get(i), colors.getGreen().get(i),
                    colors.getBlue().get(i));
            map.put(colors.getUserName().get(i), rgb);
        }
        return map;
    }

    public void setRGBColors(Map<UserId, RGB> colors) {
        ColorPopulator pop = new ColorPopulator(colors);
        this.colors = pop;
    }
}
