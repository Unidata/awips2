package com.raytheon.uf.viz.collaboration.comm.identity.roster;

import java.util.Collection;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

public interface IRosterEntry extends IRosterItem {

    /**
     * Get a collection of groups that contain this entry. If the entry is not
     * contained by a group a not null empty collection shall be returned.
     * 
     * @return Collection that contains this entry.
     */
    Collection<IRosterGroup> getGroups();

    /**
     * 
     * @return
     */
    IPresence getPresence();

    /**
     * 
     * @return
     */
    UserId getUser();
}
