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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Color information for a list of users
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012            mnash     Initial creation
 * Dec  6, 2013 2561       bclement    code cleanup
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class ColorPopulator {
    @DynamicSerializeElement
    private List<UserId> users;

    @DynamicSerializeElement
    private List<Integer> red;

    @DynamicSerializeElement
    private List<Integer> green;

    @DynamicSerializeElement
    private List<Integer> blue;

    /**
     * For serialization only, use {@link ColorPopulator#ColorPopulator(Map)}
     */
    @Deprecated
    public ColorPopulator() {
    }

    /**
     * @param rgbs
     *            Map of user ids to color information
     */
    public ColorPopulator(Map<UserId, RGB> rgbs) {
        users = new ArrayList<UserId>();
        red = new ArrayList<Integer>();
        green = new ArrayList<Integer>();
        blue = new ArrayList<Integer>();
        for (UserId key : rgbs.keySet()) {
            users.add(key);
            red.add(rgbs.get(key).red);
            green.add(rgbs.get(key).green);
            blue.add(rgbs.get(key).blue);
        }
    }

    /**
     * @return Map of user ids to color information
     */
    public Map<UserId, RGB> getColors() {
        Map<UserId, RGB> colors = new HashMap<UserId, RGB>();
        for (int i = 0; i < users.size(); i++) {
            colors.put(users.get(i),
                    new RGB(red.get(i), green.get(i), blue.get(i)));
        }
        return colors;
    }

    /**
     * @return the users
     */
    public List<UserId> getUsers() {
        return users;
    }

    /**
     * @param users
     *            the users to set
     */
    public void setUsers(List<UserId> users) {
        this.users = users;
    }

    /**
     * @return the red
     */
    public List<Integer> getRed() {
        return red;
    }

    /**
     * @param red
     *            the red to set
     */
    public void setRed(List<Integer> red) {
        this.red = red;
    }

    /**
     * @return the green
     */
    public List<Integer> getGreen() {
        return green;
    }

    /**
     * @param green
     *            the green to set
     */
    public void setGreen(List<Integer> green) {
        this.green = green;
    }

    /**
     * @return the blue
     */
    public List<Integer> getBlue() {
        return blue;
    }

    /**
     * @param blue
     *            the blue to set
     */
    public void setBlue(List<Integer> blue) {
        this.blue = blue;
    }
}
