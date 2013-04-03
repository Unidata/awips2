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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyAdapter;
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
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;

import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.actions.CopyTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.CutTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PasteTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PopupNotifier;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWord;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveFloatingView;

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

public abstract class AbstractSessionView extends CaveFloatingView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractSessionView.class);

    private static final String SESSION_IMAGE_KEY = "sessionId.key";

    private SimpleDateFormat dateFormatter = new SimpleDateFormat("HH:mm:ss");

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

    protected SessionMsgArchive msgArchive;

    private List<AlertWord> alertWords = null;

    private AudioDataStream ads = null;

    private Map<String, Font> fonts = null;

    private Map<RGB, Color> colors = null;

    private SearchComposite searchComp;

    private Action searchAction;

    protected abstract String getSessionImageName();

    protected abstract String getSessionName();

    public abstract void sendMessage();

    protected abstract void setMessageLabel(Composite comp);

    public AbstractSessionView() {
        imageMap = new HashMap<String, Image>();
        userIds = CollaborationUtils.getIds();
        fonts = new HashMap<String, Font>();
        colors = new HashMap<RGB, Color>();
        dateFormatter.setTimeZone(TimeZone.getTimeZone("UTC"));
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

    protected void initMessageArchive() {
        msgArchive = createMessageArchive();
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

        searchComp = new SearchComposite(messagesComp, SWT.BORDER);
        searchComp.hide(true);

        messagesText = new StyledText(messagesComp, SWT.MULTI | SWT.WRAP
                | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        messagesText.setLayoutData(new GridData(GridData.FILL_BOTH));

        messagesComp.addKeyListener(searchComp.getSearchKeyListener());

        messagesText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                // is it a visible character
                if (Character.isISOControl(e.character) == false) {
                    composeText.setFocus();
                    composeText.append(Character.toString(e.character));
                    composeText.setCaretOffset(composeText.getText().length());
                }
            }
        });
        // here need to grab the font from preferences and use that font
        messagesText.setFont(new Font(Display.getCurrent(), PreferenceConverter
                .getFontData(Activator.getDefault().getPreferenceStore(),
                        "font")));

        searchComp.setSearchText(messagesText);

        // adding a menu item so that Paste can be found when clicking on the
        // composeText styledtext
        MenuManager menuMgr = new MenuManager();
        menuMgr.add(new CopyTextAction(messagesText));
        Menu menu = menuMgr.createContextMenu(messagesText);
        messagesText.setMenu(menu);
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
                if (searchComp != null) {
                    searchComp.search(e);
                }
            }
        });

        // adding a menu item so that Paste can be found when clicking on the
        // composeText styledtext
        MenuManager menuMgr = new MenuManager();
        menuMgr.add(new CopyTextAction(composeText));
        menuMgr.add(new PasteTextAction(composeText));
        menuMgr.add(new CutTextAction(composeText));

        Menu menu = menuMgr.createContextMenu(composeText);
        composeText.setMenu(menu);
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
        String message = composeText.getText();
        int returnIndex = message.lastIndexOf("\n");
        message = message.substring(0, returnIndex);
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
        String subject = message.getSubject();
        appendMessage(userId, timestamp, body, subject);
    }

    public void appendMessage(final UserId userId, final long timestamp,
            final String body, final String subject) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                CollaborationConnection connection = CollaborationConnection
                        .getConnection();
                if (connection == null || body.length() == 0) {
                    // this can occur if the session has closed this method was
                    // waiting to run async, in that case just ignore the
                    // message.
                    return;
                }

                IWorkbenchSiteProgressService service = (IWorkbenchSiteProgressService) getSite()
                        .getAdapter(IWorkbenchSiteProgressService.class);
                service.warnOfContentChange();

                Date date = new Date(timestamp);
                String time = dateFormatter.format(date);

                String name = connection.getContactsManager().getDisplayName(
                        userId);

                UserId myUser = connection.getUser();
                if (!myUser.equals(userId)
                        && Activator.getDefault().getPreferenceStore()
                                .getBoolean("notifications")) {
                    createNotifier(name, time, body);
                }

                StringBuilder sb = new StringBuilder();
                if (messagesText.getCharCount() != 0) {
                    sb.append("\n");
                }
                sb.append("(").append(time).append(") ");
                int offset = sb.length();

                sb.append(name).append(": ").append(body);
                // here is the place to put the font and color changes for
                // keywords
                // read in localization file once and then don't read in again,
                // per
                // chat room?
                List<AlertWord> alertWords = retrieveAlertWords();
                List<StyleRange> ranges = new ArrayList<StyleRange>();
                if (alertWords != null) {
                    for (AlertWord keyword : alertWords) {
                        String text = keyword.getText().toLowerCase();
                        if (sb.toString().toLowerCase().contains(text)) {
                            String lowerCase = sb.toString().toLowerCase();
                            // getting the current length of the text
                            int currentLength = messagesText.getCharCount();
                            int index = lowerCase.indexOf(text);
                            while (index >= 0) {
                                Font font = null;
                                // storing off fonts so we don't leak
                                if (fonts.containsKey(keyword.getFont())) {
                                    font = fonts.get(keyword.getFont());
                                } else {
                                    FontData fd = StringConverter
                                            .asFontData(keyword.getFont());
                                    font = new Font(Display.getCurrent(), fd);
                                    fonts.put(keyword.getFont(), font);
                                }

                                RGB rgb = new RGB(keyword.getRed(), keyword
                                        .getGreen(), keyword.getBlue());
                                Color color = null;
                                // using the stored colors so we don't leak
                                if (colors.containsKey(rgb)) {
                                    color = colors.get(rgb);
                                } else {
                                    color = new Color(Display.getCurrent(), rgb);
                                    colors.put(rgb, color);
                                }
                                TextStyle style = new TextStyle(font, color,
                                        null);
                                StyleRange keywordRange = new StyleRange(style);
                                keywordRange.start = currentLength + index;
                                keywordRange.length = keyword.getText()
                                        .length();

                                ranges.add(keywordRange);
                                // compare to see if this position is already
                                // styled
                                List<StyleRange> rnges = new ArrayList<StyleRange>();
                                rnges.addAll(ranges);
                                for (StyleRange range : rnges) {
                                    if (range.start <= keywordRange.start
                                            && (range.start + range.length) >= keywordRange.start) {
                                        if (keywordRange != range) {
                                            if (range.length < keywordRange.length) {
                                                ranges.remove(range);
                                            } else {
                                                ranges.remove(keywordRange);
                                            }
                                        }
                                    }
                                }

                                // only execute things if the same user didn't
                                // type it
                                if (!myUser.equals(userId)) {
                                    executeSightsSounds(keyword);
                                }
                                // need to handle all instances of the keyword
                                // within the chat
                                index = lowerCase.indexOf(text, text.length()
                                        + index);
                            }
                        }
                    }
                }

                styleAndAppendText(sb, offset, name, userId, subject, ranges);
                msgArchive.archive(sb.toString());
                searchComp.appendText(sb.toString());
            }
        });
    }

    protected abstract void styleAndAppendText(StringBuilder sb, int offset,
            String name, UserId userId, String subject, List<StyleRange> ranges);

    protected abstract void styleAndAppendText(StringBuilder sb, int offset,
            String name, UserId userId, List<StyleRange> ranges, Color color);

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
            AudioPlayer.player.start(ads);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to find sound file",
                    e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to read sound file",
                    e);
        }
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
        parent.setLayout(new GridLayout());
        super.createPartControl(parent);
        setTitleImage(getImage());
        setPartName(getSessionName());
        initComponents(parent);
        createActions();
    }

    private void createActions() {
        searchAction = new Action("Search") {
            @Override
            public void run() {
                searchComp.toggleVisibility();
            }
        };
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        mgr.add(searchAction);
        searchAction.setImageDescriptor(IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "find.gif"));
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

    private void createNotifier(String id, String time, String body) {
        String titleText = "(" + time + ") " + id;
        PopupNotifier.notify(titleText, body);
    }

    @Subscribe
    public void changeFont(FontData data) {
        messagesText.setFont(new Font(Display.getCurrent(), data));
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

    protected abstract SessionMsgArchive createMessageArchive();

    protected void sendErrorMessage(StringBuilder sb) {
        Color color = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
        sendGenericMessage(sb, color);
    }

    protected void sendSystemMessage(StringBuilder sb) {
        Color color = Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
        sendGenericMessage(sb, color);
    }

    private void sendGenericMessage(final StringBuilder string,
            final Color color) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                Date date = new Date();
                String time = dateFormatter.format(date);
                string.insert(0, "(" + time + ") : ");
                if (messagesText.getCharCount() != 0) {
                    string.insert(0, "\n");
                }
                StyleRange range = new StyleRange(messagesText.getCharCount(),
                        string.length(), color, null, SWT.BOLD);
                List<StyleRange> ranges = new ArrayList<StyleRange>();
                ranges.add(range);
                styleAndAppendText(string, 0, string.toString(), null, ranges,
                        color);
                msgArchive.archiveLine(string.toString());
                searchComp.appendText(string.toString());
            }
        });
    }

}
