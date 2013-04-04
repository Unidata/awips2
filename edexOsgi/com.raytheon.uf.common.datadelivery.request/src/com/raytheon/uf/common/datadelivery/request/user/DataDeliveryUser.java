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
package com.raytheon.uf.common.datadelivery.request.user;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@DynamicSerialize
public class DataDeliveryUser implements IUser {

    @DynamicSerializeElement
    private DataDeliveryUserId userId;

    @DynamicSerializeElement
    private AuthenticationData authenticationData;

    public DataDeliveryUser() {

    }

    public DataDeliveryUser(String userId) {
        setUserId(new DataDeliveryUserId(userId));
    }

    public DataDeliveryUserId getUserId() {
        return userId;
    }

    public void setUserId(DataDeliveryUserId userId) {
        this.userId = userId;
    }

    public AuthenticationData getAuthenticationData() {
        return authenticationData;
    }

    public void setAuthenticationData(AuthenticationData authenticationData) {
        this.authenticationData = authenticationData;
    }

    @Override
    public DataDeliveryUserId uniqueId() {
        return userId;
    }

    @Override
    public IAuthenticationData authenticationData() {
        // TODO Auto-generated method stub
        return this.authenticationData;
    }

    @Override
    public String toString() {
        return this.getClass().getName() + "[userId = " + userId.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((authenticationData == null) ? 0 : authenticationData
                        .hashCode());
        result = prime * result + ((userId == null) ? 0 : userId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DataDeliveryUser other = (DataDeliveryUser) obj;
        if (authenticationData == null) {
            if (other.authenticationData != null) {
                return false;
            }
        } else if (!authenticationData.equals(other.authenticationData)) {
            return false;
        }
        if (userId == null) {
            if (other.userId != null) {
                return false;
            }
        } else if (!userId.equals(other.userId)) {
            return false;
        }
        return true;
    }
}
