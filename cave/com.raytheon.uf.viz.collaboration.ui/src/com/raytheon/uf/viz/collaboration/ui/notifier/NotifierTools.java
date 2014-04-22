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
package com.raytheon.uf.viz.collaboration.ui.notifier;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smack.packet.Presence.Type;

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
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.core.sounds.SoundUtil;

/**
 * Contact Notifier Utility Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2014   2632     mpduff      Initial creation.
 * Mar 05, 2014   2632     mpduff      Removed unused field.
 * Mar 12, 2014   2632     mpduff      Don't process the notifier if the presence is null.
 * Apr 02, 2014   2632     mpduff      Remove user from list every time on a sign off
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NotifierTools {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotifierTools.class);

    /** Set of user names of users currently online */
    private static Set<String> usersOnline = new HashSet<String>();

    /** Contact notifier config file */
    private static final String NOTIFIER_FILE_PATH = "collaboration/contactNotifiers.xml";

    /** Sound directory in localization */
    private static final String SOUND_DIR = "collaboration/sounds";

    /** The contact notifier localization file */
    private static LocalizationFile taskFile;

    /**
     * Process appropriate notifiers for this presence change.
     * 
     * @param presence
     *            The updated Presence
     */
    public static void processNotifiers(Presence presence) {
        if (presence == null) {
            return;
        }

        UserId userId = IDConverter.convertFrom(presence.getFrom());
        NotifierTask task = NotifierTools.getNotifierTask(userId.getName());
        if (task != null && task.getUserName().equals(userId.getName())) {
            Presence rosterPresence = CollaborationConnection.getConnection()
                    .getContactsManager().getPresence(userId);
            Mode mode = rosterPresence.getMode();
            Type type = presence.getType();
            Set<Notifier> notifiers = task.getNotifierList();

            // Sign on/off events
            if (!usersOnline.contains(userId.getName())
                    && Type.available == type) {
                usersOnline.add(userId.getName());
                if (notifiers.contains(Notifier.SignOn)) {
                    executeNotifierTask(task);
                    NotifierTools.taskExecuted(task);
                    return;
                }
            } else if (usersOnline.contains(userId.getName())
                    && (Type.unavailable == type)) {
                usersOnline.remove(userId.getName());
                if (notifiers.contains(Notifier.SignOff)) {
                    executeNotifierTask(task);
                    NotifierTools.taskExecuted(task);
                    return;
                }
            }

            // presence mode change events
            for (Notifier n : notifiers) {
                if (Notifier.Returns == n && mode == null
                        && Type.available == type) {
                    executeNotifierTask(task);
                    NotifierTools.taskExecuted(task);
                    break;
                } else if (Notifier.Away == n
                        && (Mode.away == mode || Mode.xa == mode || Mode.dnd == mode)) {
                    executeNotifierTask(task);
                    NotifierTools.taskExecuted(task);
                    break;
                }
            }
        }
    }

    /**
     * Execute the NotifierTask.
     * 
     * @param task
     *            The NotifierTask to execute
     */
    private static void executeNotifierTask(NotifierTask task) {
        if (task.isSoundValid()) {
            SoundUtil.playSound(task.getSoundFilePath());
        }
    }

    /**
     * Save the notifiers.
     * 
     * @param taskList
     *            List of NotifierTasks
     * @return true if saved successfully
     */
    public static boolean saveNotifiers(List<NotifierTask> taskList) {
        for (NotifierTask notifier : taskList) {
            String soundPath = notifier.getSoundFilePath();
            if (soundPath != null && !soundPath.isEmpty()) {
                String path = copySoundFile(soundPath);
                // Reset sound file path the localization
                notifier.setSoundFilePath(path);
            }
        }

        NotifierTaskXML wrapper = new NotifierTaskXML();
        wrapper.setNotifierTasks(taskList.toArray(new NotifierTask[0]));

        LocalizationFile file = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        file = PathManagerFactory.getPathManager().getLocalizationFile(context,
                NOTIFIER_FILE_PATH);
        JAXB.marshal(wrapper, file.getFile());
        try {
            file.save();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to save Contact Notifiers to localization", e);
            return false;
        }

        return true;
    }

    /**
     * Get all the configured NotifierTasks.
     * 
     * @return List of NotifierTasks
     */
    public static synchronized List<NotifierTask> getNotifierTasks() {
        List<NotifierTask> taskList = new ArrayList<NotifierTask>();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        taskFile = PathManagerFactory.getPathManager().getLocalizationFile(
                context, NOTIFIER_FILE_PATH);
        if (taskFile.exists() || taskFile.getFile().exists()) {
            NotifierTaskXML wrapper = JAXB.unmarshal(taskFile.getFile(),
                    NotifierTaskXML.class);
            if (wrapper != null) {
                for (NotifierTask task : wrapper.getNotifierTasks()) {
                    taskList.add(task);
                }
            }
        }

        return taskList;
    }

    /**
     * Copy the sound file to cave static user level localization.
     * 
     * @param soundPath
     *            The path to the file to copy
     * @return The new path
     */
    private static String copySoundFile(String soundPath) {
        String[] dirs = soundPath.split(File.separator);
        String filename = dirs[dirs.length - 1];
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile lFile = pm.getLocalizationFile(context,
                FileUtil.join(SOUND_DIR, filename));
        if (lFile.exists() && !soundPath.equals(lFile.getFile().getPath())) {
            MessageBox messageBox = new MessageBox(Display.getCurrent()
                    .getActiveShell(), SWT.YES | SWT.NO);
            messageBox.setText("File exists");
            messageBox
                    .setMessage("File already exists, overwrite the existing file? ");
            int answer = messageBox.open();
            if (answer == SWT.YES) {
                return copyFile(soundPath, lFile);
            }
        }
        if (!lFile.exists()) {
            return copyFile(soundPath, lFile);
        }

        return soundPath;
    }

    private static String copyFile(String path, LocalizationFile lFile) {
        File soundFile = new File(path);

        File destination = lFile.getFile();
        try {
            FileUtil.copyFile(soundFile, destination);
            lFile.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error Occured copying sound file", e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error Occured copying sound file", e);
        }

        return destination.getAbsolutePath();
    }

    /**
     * Get a notifier task for the specified user name.
     * 
     * @param name
     *            The user name
     * @return The NotifierTask or null if none
     */
    public static NotifierTask getNotifierTask(String name) {
        for (NotifierTask task : getNotifierTasks()) {
            if (task.getUserName().equals(name)) {
                return task;
            }
        }

        return null;
    }

    public static synchronized void taskExecuted(NotifierTask task) {
        if (!task.isRecurring()) {
            List<NotifierTask> tasks = getNotifierTasks();
            tasks.remove(task);
            saveNotifiers(tasks);
        }
    }
}
