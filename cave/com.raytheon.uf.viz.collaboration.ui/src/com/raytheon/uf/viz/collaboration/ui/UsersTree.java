package com.raytheon.uf.viz.collaboration.ui;

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

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.viz.collaboration.data.CollaborationUser;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTree {
    private List<UsersTree> children;

    private String text;

    private CollaborationUser node;

    public UsersTree(String text) {
        this.text = text;
        this.children = new LinkedList<UsersTree>();
        this.node = null;
    }

    public UsersTree(CollaborationUser user) {
        this.text = user.getId();
        this.children = null;
        this.node = user;
    }

    public UsersTree addChild(String text) {
        UsersTree child = new UsersTree(text);
        children.add(child);
        return child;
    }

    public UsersTree addChild(CollaborationUser user) {
        UsersTree child = new UsersTree(user);
        children.add(child);
        return child;
    }

    public UsersTree findChildByText(String text) {
        CollaborationUser user = new CollaborationUser(text);
        if (hasChildren()) {
            for (UsersTree child : children) {
                if (user.compareTo(child.getUser()) == 0) {
                    return child;
                }
            }
        }
        return null;
    }

    public List<UsersTree> getChildren() {
        return this.children;
    }

    public boolean hasChildren() {
        return children != null && children.size() > 0;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getText() {
        return this.text;
    }

    public CollaborationUser getUser() {
        return this.node;
    }
}
