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
package com.raytheon.uf.viz.collaboration.ui;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.presence.IPresence.Mode;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.ecf.presence.roster.Roster;
import org.eclipse.ecf.presence.roster.RosterEntry;
import org.eclipse.ecf.presence.roster.RosterGroup;
import org.eclipse.swt.graphics.Image;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserIdWrapper;
import com.raytheon.uf.viz.collaboration.data.AlertWord;
import com.raytheon.uf.viz.collaboration.data.AlertWordWrapper;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Methods for sending, receiving messages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationUtils {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationUtils.class);

    public static final org.eclipse.ecf.presence.IPresence.Mode[] statusModes = {
            Mode.AVAILABLE, Mode.DND, Mode.AWAY };

    private static final String PREFIX_CONFERENCE = "conference.";

    /**
     * Get an image associated with the node.
     * 
     * @param node
     * @return image
     */
    public static Image getNodeImage(String name) {
        return IconUtil.getImageDescriptor(Activator.getDefault().getBundle(),
                name.toLowerCase() + ".gif").createImage();
    }

    public static Collection<Object> readAliases() {
        UserId[] ids = getIds();
        Roster roster = (Roster) CollaborationDataManager.getInstance()
                .getCollaborationConnection(true).getRosterManager()
                .getRoster();
        Collection<?> rosterObjects = new ArrayList<Object>();
        rosterObjects.addAll(roster.getItems());
        for (Object ob : rosterObjects) {
            if (ob instanceof IRosterEntry) {
                IRosterEntry entry = (IRosterEntry) ob;
                for (UserId id : ids) {
                    UserId sId = IDConverter.convertFrom(entry.getUser());
                    if (sId.equals(id)) {
                        sId.setAlias(id.getAlias());
                        entry = new RosterEntry(entry.getParent(), sId,
                                entry.getPresence());
                    }
                }
            } else if (ob instanceof IRosterGroup) {
                Collection<Object> obs = new ArrayList<Object>();
                obs.addAll(((IRosterGroup) ob).getEntries());
                // System.out.println("\n\n\n" + ob);
                for (Object entryOb : obs) {
                    IRosterEntry entry = (IRosterEntry) entryOb;
                    // System.out.println(entry.getName());
                    for (UserId id : ids) {
                        UserId sId = IDConverter.convertFrom(entry.getUser());
                        if (sId.equals(id)) {
                            sId.setAlias(id.getAlias());
                            ((RosterGroup) ob).remove(entry);
                            entry = new RosterEntry(entry.getParent(), sId,
                                    entry.getPresence());
                            break;
                        }
                    }
                }
            }
        }

        return roster.getItems();
    }

    public static UserId[] getIds() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(
                        context,
                        "collaboration" + File.separator
                                + "collaborationAliases.xml");
        if (file.exists()) {
            UserIdWrapper ids = (UserIdWrapper) JAXB.unmarshal(file.getFile(),
                    UserIdWrapper.class);
            if (ids.getUserIds() == null) {
                return new UserId[0];
            }
            return ids.getUserIds();
        }
        return new UserId[0];
    }

    public static void addAlias() {
        Job job = new Job("Saving aliases to file") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationContext context = pm.getContext(
                        LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
                LocalizationFile file = pm.getLocalizationFile(context,
                        "collaboration" + File.separator
                                + "collaborationAliases.xml");
                IRoster roster = CollaborationDataManager.getInstance()
                        .getCollaborationConnection(true).getRosterManager()
                        .getRoster();
                Set<IUser> ids = new HashSet<IUser>();

                // get the entries that are alone
                for (Object ob : roster.getItems()) {
                    if (ob instanceof IRosterEntry) {
                        IRosterEntry entry = (IRosterEntry) ob;
                        IUser id = entry.getUser();
                        if (id instanceof User) {
                            if (id.getNickname() != null
                                    && !id.getNickname().isEmpty()) {
                                ids.add(IDConverter.convertFrom(id));
                            }
                        } else if (id instanceof UserId) {
                            UserId uid = (UserId) id;
                            if (uid.getAlias() != null
                                    && !uid.getAlias().isEmpty()) {
                                ids.add(uid);
                            }
                        }
                    } else if (ob instanceof IRosterGroup) {
                        for (Object entryOb : ((IRosterGroup) ob).getEntries()) {
                            IRosterEntry entry = (IRosterEntry) entryOb;
                            IUser id = entry.getUser();
                            if (id instanceof User) {
                                if (id.getNickname() != null
                                        && !id.getNickname().isEmpty()) {
                                    ids.add(IDConverter.convertFrom(id));
                                }
                            } else if (id instanceof UserId) {
                                UserId uid = (UserId) id;
                                if (uid.getAlias() != null
                                        && !uid.getAlias().isEmpty()) {
                                    ids.add(uid);
                                }
                            }
                        }
                    }
                }

                // a wrapper for JAXB
                UserIdWrapper wrapper = new UserIdWrapper();
                wrapper.setUserIds(ids.toArray(new UserId[0]));
                try {
                    JAXB.marshal(wrapper, file.getFile());
                    file.save();
                    return Status.OK_STATUS;
                } catch (LocalizationOpFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    return Status.CANCEL_STATUS;
                }
            }
        };
        job.schedule();
    }

    public static String formatMode(Mode mode) {
        String name = mode.toString();

        StringBuilder result = new StringBuilder(name.length());
        String[] words = name.split("\\s");
        for (int i = 0, l = words.length; i < l; ++i) {
            if (i > 0)
                result.append(" ");
            result.append(Character.toUpperCase(words[i].charAt(0))).append(
                    words[i].substring(1));

        }

        return result.toString();
    }

    public static List<AlertWord> getAlertWords() {
        LocalizationFile file = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        file = PathManagerFactory.getPathManager().getLocalizationFile(context,
                "collaboration" + File.separator + "alertWords.xml");
        if (file.exists() || file.getFile().exists()) {
            AlertWordWrapper words = (AlertWordWrapper) JAXB.unmarshal(
                    file.getFile(), AlertWordWrapper.class);
            if (words.getAlertWords() == null) {
                return new ArrayList<AlertWord>();
            } else {
                List<AlertWord> alertWords = new ArrayList<AlertWord>();
                for (int i = 0; i < words.getAlertWords().length; i++) {
                    alertWords.add(words.getAlertWords()[i]);
                }
                return alertWords;
            }
        } else {
            return new ArrayList<AlertWord>();
        }
    }

    public static void saveAlertWords(List<AlertWord> words) {
        LocalizationFile file = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        file = PathManagerFactory.getPathManager().getLocalizationFile(context,
                "collaboration" + File.separator + "alertWords.xml");

        // save the sound file in the correct place if not there
        for (AlertWord word : words) {
            String soundPath = word.getSoundPath();
            if (soundPath != null && !soundPath.isEmpty()) {
                String[] dirs = soundPath.split(File.separator);
                String filename = dirs[dirs.length - 1];
                LocalizationFile lFile = pm.getLocalizationFile(context,
                        "collaboration" + File.separator + "sounds"
                                + File.separator + filename);
                if (!lFile.exists()) {
                    File soundFile = new File(soundPath);

                    File destination = lFile.getFile();
                    soundFile.renameTo(destination);
                }
            }
        }

        AlertWordWrapper wrapper = new AlertWordWrapper();
        wrapper.setAlertWords(words.toArray(new AlertWord[0]));
        JAXB.marshal(wrapper, file.getFile());
        try {
            file.save();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to save alert words to localization", e);
        }
    }
}