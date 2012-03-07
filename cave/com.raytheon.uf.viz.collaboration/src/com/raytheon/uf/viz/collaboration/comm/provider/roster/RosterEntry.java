package com.raytheon.uf.viz.collaboration.comm.provider.roster;

import java.util.ArrayList;
import java.util.Collection;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;

public class RosterEntry extends RosterItem implements IRosterEntry, IMutableRosterEntry {

    private IChatID userId = null;
    
    private IPresence presence = null;

    private Collection<IRosterGroup> groups = null;

    /**
     * 
     * @param id
     */
    public RosterEntry(IChatID id) {
        userId = id;
        groups = new ArrayList<IRosterGroup>();
    }

    /**
     * 
     */
    @Override
    public IChatID getUser() {
        return userId;
    }    

    /**
     * 
     * @param group
     */
    public void addGroup(IRosterGroup group) {
        groups.add(group);
    }

    /**
     * 
     */
    @Override
    public Collection<IRosterGroup> getGroups() {
        return null;
    }

    /**
     * 
     * @param presence
     */
    public void setPresence(IPresence presence) {
        this.presence = presence;
    }
    
    /**
     * 
     * @return 
     */
    @Override
    public IPresence getPresence() {
        return presence;
    }

    
    public static final void main(String [] args) {
        
        IChatID id = new IChatID() {

            private String name = null;
            
            private String nickName = null;
            
            private String host = null;
            
            private String resource = null;
            
            @Override
            public void setName(String userName) {
                name = userName;
            }

            @Override
            public String getName() {
                return name;
            }

            @Override
            public void setNickname(String nickname) {
                nickName = nickname;
            }

            @Override
            public String getNickname() {
                return nickName;
                
            }
            
            @Override
            public void setHost(String hostName) {
                host = hostName;
            }

            @Override
            public String getHost() {
                return host;
            }
            
            @Override
            public String getResource() {
                return resource;
            }
            
            @Override
            public void setResource(String resource) {
                this.resource = resource;
            }
            

            @Override
            public String getFQName() {
                StringBuilder sb = new StringBuilder(name);
                sb.append("@");
                sb.append(host);
                if(resource != null) {
                    sb.append("/");
                    sb.append(resource);
                }
                
                
                return sb.toString();
            }
            
        };

        id.setName("fred");
        id.setHost("awipscm.omaha.us.ray.com");
        id.setResource("smack");
        
        IMutableRosterEntry entry = new RosterEntry(id);
        entry.setPresence(new Presence());
        
        IRosterEntry en = entry;
        
        System.out.println(id.getFQName());
    }
    
    
}
