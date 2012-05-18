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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;

import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.AlertWord;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.notification.notifier.PopupNotifier;

/**
 * This performs most of the work for creating a View for a peer-to-peer or
 * multi-user session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012 244        rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AbstractSessionView extends ViewPart {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractSessionView.class);

    private static final String SESSION_IMAGE_KEY = "sessionId.key";

    /**
     * Mapping of images used in the view so they are not constantly created and
     * allowing them to be disposed.
     */
    protected Map<String, Image> imageMap;

    private static int SASH_WIDTH = 5;

    private static int SASH_COLOR = SWT.COLOR_DARK_GRAY;

    protected StyledText messagesText;

    private StyledText composeText;

    private UserId[] userIds = null;

    private SessionMsgArchive msgArchive;
    
    private List<AlertWord> alertWords = null;

    private AudioDataStream ads = null;

    private Map<String, Font> fonts = null;

    private Map<RGB, Color> colors = null;

    protected abstract String getSessionImageName();

    protected abstract String getSessionName();

    public abstract void sendMessage();

    protected abstract void setMessageLabel(Composite comp);

    public AbstractSessionView() {
        imageMap = new HashMap<String, Image>();
        userIds = CollaborationUtils.getIds();
        fonts = new HashMap<String, Font>();
        colors = new HashMap<RGB, Color>();
    }

    protected void initComponents(Composite parent) {
        Composite sashComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashComp.setLayout(layout);
        sashComp.setLayoutData(data);

        Color sashColor = Display.getCurrent().getSystemColor(SASH_COLOR);

        SashForm sashForm = new SashForm(sashComp, SWT.VERTICAL);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayout(layout);
        sashForm.setLayoutData(data);
        sashForm.setBackground(sashColor);
        sashForm.setSashWidth(SASH_WIDTH);

        populateSashForm(sashForm);
    }

    /**
     * A Subclass must override this method to set sashForm's weight and to add
     * other components.
     * 
     * @param sashForm
     */
    protected void populateSashForm(SashForm sashForm) {
        createMessagesComp(sashForm);
        createComposeComp(sashForm);
    }

    private void createMessagesComp(Composite parent) {
        Composite messagesComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        messagesComp.setLayout(layout);
        setMessageLabel(messagesComp);
        messagesText = new StyledText(messagesComp, SWT.MULTI | SWT.WRAP
                | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        messagesText.setLayoutData(new GridData(GridData.FILL_BOTH));
        messagesText.addFocusListener(new FocusListener() {
            @Override
            public void focusLost(FocusEvent e) {
            }

            @Override
            public void focusGained(FocusEvent e) {
                composeText.setFocus();
            }
        });
    }

    protected void createComposeComp(Composite parent) {
        Composite composeComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        composeComp.setLayout(layout);

        Label label = new Label(composeComp, SWT.NONE);
        label.setText("Compose:");
        composeText = new StyledText(composeComp, SWT.MULTI | SWT.WRAP
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        composeText.setLayoutData(new GridData(GridData.FILL_BOTH));
        composeText.setToolTipText("Enter message here");
        composeText.addKeyListener(new KeyListener() {
            private boolean keyPressed;

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.SHIFT) {
                    keyPressed = false;
                }
                // do nothing, all done on key pressed
            }

            @Override
            public void keyPressed(KeyEvent e) {
                if (!keyPressed
                        && (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR)) {
                    sendMessage();
                }
                if (e.keyCode == SWT.SHIFT) {
                    keyPressed = true;
                }
            }
        });
    }

    private Image getImage() {
        Image image = imageMap.get(SESSION_IMAGE_KEY);
        if (image == null) {
            image = IconUtil.getImageDescriptor(
                    Activator.getDefault().getBundle(), getSessionImageName())
                    .createImage();
            if (image != null) {
                imageMap.put(SESSION_IMAGE_KEY, image);
            }
        }
        return image;
    }

    /**
     * Get the composed message and clear the text.
     * 
     * @return message
     */
    protected String getComposedMessage() {
        String message = composeText.getText().trim();
        composeText.setText("");
        composeText.setCaretOffset(0);
        return message;
    }

    /**
     * Append the message into the message text field.
     * 
     * @param message
     */
    public void appendMessage(IMessage message) {
        UserId userId = (UserId) message.getFrom();
        long timestamp = message.getTimeStamp();
        String body = message.getBody();
        appendMessage(userId, timestamp, body);
    }

    public void appendMessage(UserId userId, long timestamp, String body) {
        IWorkbenchSiteProgressService service = (IWorkbenchSiteProgressService) getSite()
                .getAdapter(IWorkbenchSiteProgressService.class);
        service.warnOfContentChange();

        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(timestamp);
        String time = String.format("%1$tI:%1$tM:%1$tS %1$Tp", cal);

        if (!CollaborationDataManager.getInstance()
                .getCollaborationConnection(true).getUser().equals(userId)
                && Activator.getDefault().getPreferenceStore()
                        .getBoolean("notifications")) {
            createNotifier(userId, time, body);
        }

        String name = userId.getName();
        for (UserId id : userIds) {
            if (id.equals(userId)) {
                name = id.getAlias();
                break;
            }
        }

        StringBuilder sb = new StringBuilder();
        if (messagesText.getCharCount() != 0) {
            sb.append("\n");
        }
        sb.append("(").append(time).append(") ");
        int offset = sb.length();

        sb.append(name).append(": ").append(body);
        // here is the place to put the font and color changes for keywords
        // read in localization file once and then don't read in again, per
        // chat room?
        List<AlertWord> alertWords = retrieveAlertWords();
        List<StyleRange> ranges = new ArrayList<StyleRange>();
        if (alertWords != null) {
            for (AlertWord keyword : alertWords) {
                if (sb.toString().toLowerCase()
                        .contains(keyword.getText().toLowerCase())) {
                    Font font = null;
                    if (fonts.containsKey(keyword.getFont())) {
                        font = fonts.get(keyword.getFont());
                    } else {
                        FontData fd = StringConverter.asFontData(keyword
                                .getFont());
                        font = new Font(Display.getCurrent(), fd);
                        fonts.put(keyword.getFont(), font);
                    }
                    RGB rgb = new RGB(keyword.getRed(), keyword.getGreen(),
                            keyword.getBlue());
                    Color color = null;
                    if (colors.containsKey(rgb)) {
                        color = colors.get(rgb);
                    } else {
                        color = new Color(Display.getCurrent(), rgb);
                        colors.put(rgb, color);
                    }
                    TextStyle style = new TextStyle(font, color, null);
                    StyleRange keywordRange = new StyleRange(style);
                    keywordRange.start = messagesText.getCharCount()
                            + sb.toString().toLowerCase()
                                    .indexOf(keyword.getText().toLowerCase());
                    keywordRange.length = keyword.getText().length();

                    // compare to see if this position is already styled
                    // List<StyleRange> rnges = new ArrayList<StyleRange>();
                    // rnges.addAll(ranges);
                    // for (StyleRange range : rnges) {
                    // if (range.start >= keywordRange.start
                    // && (range.start + range.length) >= (keywordRange.start))
                    // {
                    // if (range.length < keywordRange.length) {
                    // ranges.remove(range);
                    // ranges.add(keywordRange);
                    // } else {
                    // ranges.add(keywordRange);
                    // }
                    // }
                    // }
                    ranges.add(keywordRange);
                    executeSightsSounds(keyword);
                }
            }
        }

        styleAndAppendText(sb, offset, name, userId, ranges);
        if (msgArchive == null) {
            msgArchive = getMessageArchive();
        }
        msgArchive.archive(sb.toString());
    }

    protected abstract void styleAndAppendText(StringBuilder sb, int offset,
            String name, UserId userId, List<StyleRange> ranges);

    /**
     * Find keys words in body of message starting at offset. /**
     * 
     * @param builder
     * @param offset
     * @return alertWords
     */
    protected List<AlertWord> retrieveAlertWords() {
        if (alertWords == null) {
            alertWords = CollaborationUtils.getAlertWords();
        }
        return alertWords;
    }

    /**
     * Place holder must override to do something.
     */
    protected void executeSightsSounds(AlertWord word) {
        String filename = word.getSoundPath();
        if (filename == null || filename.isEmpty()) {
            return;
        }
        File soundFile = new File(filename);
        InputStream in;
        AudioStream as = null;
        AudioData data = null;
        try {
            if (ads != null) {
                AudioPlayer.player.stop(ads);
            }
            in = new FileInputStream(soundFile);
            as = new AudioStream(in);
            data = as.getData();
            ads = new AudioDataStream(data);
            Field field = null;
            try {
                field = AudioPlayer.player.getClass().getDeclaredField("DEBUG");
                field.setAccessible(true);
                field.setBoolean(field, true);
            } catch (SecurityException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (NoSuchFieldException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            AudioPlayer.player.start(ads);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to find sound file",
                    e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to read sound file",
                    e);
        }
        System.out.println("\n\nNew\n\n");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        setTitleImage(getImage());
        setPartName(getSessionName());
        initComponents(parent);
    }

    @Override
    public void dispose() {
        for (Image im : imageMap.values()) {
            im.dispose();
        }

        for (Font font : fonts.values()) {
            font.dispose();
        }

        for (Color color : colors.values()) {
            color.dispose();
        }

        imageMap.clear();
        imageMap = null;
        alertWords = null;
      if (msgArchive != null) {
            msgArchive.close();
            msgArchive = null;
        }

        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        composeText.setFocus();
    }

    private void createNotifier(UserId id, String time, String body) {
        String text = id.getName();
        for (UserId uid : userIds) {
            if (uid.equals(id)) {
                text = uid.getAlias();
                break;
            }
        }
        String titleText = "(" + time + ") " + text;
        PopupNotifier.notify(titleText, body);
    }

    /**
     * @return the userIds
     */
    public UserId[] getUserIds() {
        return userIds;
    }

    /**
     * @param userIds
     *            the userIds to set
     */
    public void setUserIds(UserId[] userIds) {
        this.userIds = userIds;
    }

     public void setAlertWords(List<AlertWord> words) {
        alertWords = words;
    }
   
    protected abstract SessionMsgArchive getMessageArchive();
}
