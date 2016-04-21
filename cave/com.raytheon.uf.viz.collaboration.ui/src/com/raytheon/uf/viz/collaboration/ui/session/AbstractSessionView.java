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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.actions.CopyTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.CutTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PasteTextAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PopupNotifier;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWord;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.core.sounds.SoundUtil;
import com.raytheon.uf.viz.spellchecker.text.SpellCheckTextViewer;
import com.raytheon.viz.ui.VizWorkbenchManager;
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
 * Dec 19, 2013 2563       bclement    moved color lookup into runAsync block
 * Jan 30, 2014 2698       bclement    get display name from child class
 * Feb 13, 2014 2751       bclement    made generic
 * Feb 18, 2014 2631       mpduff      Add ability to play sounds on join actions
 * Feb 24, 2014 2632       mpduff      Moved sound generation code to CollaborationUtils
 * Mar 06, 2014 #2865      lvenable    Fixed font memory leaks added SWT dispose checks when
 *                                     running in an asynchronous thread.
 * Mar 11, 2014 #2865      lvenable    Added null checks for msgArchive.
 * Jun 20, 2014 3281       bclement    made sendErrorMessage() public
 * Jun 27, 2014 3323       bclement    fixed disposed font issue
 * Oct 09, 2014 3711       mapeters    Display chat text in accordance with preferences.
 * Oct 14, 2014 3709       mapeters    Support changing foreground/background color.
 * Nov 14, 2014 3709       mapeters    Changing foreground/background colors no longer 
 *                                     implemented here, added messagesTextMenuMgr.
 * Nov 26, 2014 3709       mapeters    Added {@link #getColorFromRGB()}.
 * Dec 08, 2014 3709       mapeters    Removed messagesTextMenuMgr.
 * Jan 13, 2015 3709       bclement    styleAndAppendText() takes foreground and background
 * Mar 24, 2015 4265       mapeters    Implement common styleAndAppendText()s here, apply
 *                                     most general StyleRange to text first.
 * Mar 24, 2015 4316       mapeters    Display date of message if new day or user-preferred
 * Mar 27, 2015 4327       mapeters    Added task bar notification for received messages
 * Apr 14, 2015 4362       mapeters    Added spell checking to chat input box.
 * May 22, 2015 4328       mapeters    Change icon of non-visible tabs that receive message
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AbstractSessionView<T extends IUser> extends
        CaveFloatingView {

    private static ThreadLocal<SimpleDateFormat> timeFormatter = TimeUtil
            .buildThreadLocalSimpleDateFormat("HH:mm:ss",
                    TimeUtil.GMT_TIME_ZONE);

    private static ThreadLocal<SimpleDateFormat> dateFormatter = TimeUtil
            .buildThreadLocalSimpleDateFormat("yyyy-MM-dd ",
                    TimeUtil.GMT_TIME_ZONE);

    /**
     * Mapping of images used in the view so they are not constantly created and
     * allowing them to be disposed.
     */
    protected Map<String, Image> imageMap;

    private static int SASH_WIDTH = 5;

    private static int SASH_COLOR = SWT.COLOR_DARK_GRAY;

    protected StyledText messagesText;

    /** Font used with the messagesText control. */
    private Font messagesTextFont;

    private SpellCheckTextViewer composeTextViewer;

    protected SessionMsgArchive msgArchive;

    private List<AlertWord> alertWords = null;

    private Map<String, Font> fonts = null;

    private Map<RGB, Color> colors = null;

    private SearchComposite searchComp;

    private Action searchAction;

    private Date lastMessageDay;

    protected abstract String getSessionImageName();

    protected abstract String getNotificationImageName();

    protected abstract String getSessionName();

    public abstract void sendMessage();

    protected abstract void setMessageLabel(Composite comp);

    public AbstractSessionView() {
        imageMap = new HashMap<>();
        fonts = new HashMap<>();
        colors = new HashMap<>();
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
                    StyledText composeText = composeTextViewer.getTextWidget();
                    composeText.setFocus();
                    composeText.append(Character.toString(e.character));
                    composeText.setCaretOffset(composeText.getText().length());
                }
            }
        });

        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        // here need to grab the font from preferences and use that font
        messagesTextFont = new Font(Display.getCurrent(),
                org.eclipse.jface.preference.PreferenceConverter.getFontData(
                        store, "font"));
        messagesText.setFont(messagesTextFont);

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

        composeTextViewer = new SpellCheckTextViewer(composeComp, SWT.MULTI
                | SWT.WRAP | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        StyledText composeText = composeTextViewer.getTextWidget();

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
        List<IAction> menuItems = new ArrayList<>();
        menuItems.add(new CopyTextAction(composeText));
        menuItems.add(new PasteTextAction(composeText));
        menuItems.add(new CutTextAction(composeText));
        composeTextViewer.addMenuItems(menuItems.toArray(new IAction[0]));
    }

    private Image getImage(String imageName) {
        Image image = imageMap.get(imageName);
        if (image == null) {
            image = IconUtil.getImageDescriptor(
                    Activator.getDefault().getBundle(), imageName)
                    .createImage();
            if (image != null) {
                imageMap.put(imageName, image);
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
        StyledText composeText = composeTextViewer.getTextWidget();
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
    @SuppressWarnings("unchecked")
    public void appendMessage(IMessage message) {
        T userId = (T) message.getFrom();
        long timestamp = message.getTimeStamp();
        String body = message.getBody();
        String subject = message.getSubject();
        appendMessage(userId, timestamp, body, subject);
    }

    protected abstract String getDisplayName(T userId);

    public void appendMessage(final T userId, final long timestamp,
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
                String time = timeFormatter.get().format(date);

                String name = getDisplayName(userId);

                UserId myUser = connection.getUser();
                if (!myUser.isSameUser(userId)) {
                    // If user-preferred, notify with popup
                    if (Activator.getDefault().getPreferenceStore()
                            .getBoolean("notifications")) {
                        createNotifier(name, time, body);
                    }

                    // Bold/flash task bar item for windows out of focus if
                    // peer-to-peer chat or user-preferred for group chats
                    if (shouldNotifyTaskbar()) {
                        getSite().getShell().forceActive();
                    }

                    // Change tab icon if the tab isn't visible
                    IWorkbenchPage activePage = VizWorkbenchManager
                            .getInstance().getCurrentWindow().getActivePage();
                    if (!activePage.isPartVisible(AbstractSessionView.this)) {
                        Image notificationImage = getImage(getNotificationImageName());
                        AbstractSessionView.this
                                .setTitleImage(notificationImage);
                    }
                }

                StringBuilder sb = new StringBuilder();

                if (messagesText.isDisposed() == false
                        && messagesText.getCharCount() != 0) {
                    sb.append("\n");
                }

                boolean newDay = storeAndCompareTimestamp(date);
                if (Activator.getDefault().getPreferenceStore()
                        .getBoolean("displayDate")
                        || newDay) {
                    time = dateFormatter.get().format(date) + time;
                }

                sb.append("(").append(time).append(") ");
                int offset = sb.length();

                boolean newLine = Activator.getDefault().getPreferenceStore()
                        .getBoolean("chatLines");
                String displayPreference = newLine ? ("\n      ") : (": ");

                sb.append(name).append(displayPreference).append(body);
                // here is the place to put the font and color changes for
                // keywords
                // read in localization file once and then don't read in again,
                // per
                // chat room?
                if (messagesText.isDisposed() == false) {
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
                                        font = new Font(Display.getCurrent(),
                                                fd);
                                        fonts.put(keyword.getFont(), font);
                                    }

                                    RGB rgb = new RGB(keyword.getRed(), keyword
                                            .getGreen(), keyword.getBlue());

                                    // using the stored colors so we don't leak
                                    Color color = getColorFromRGB(rgb);

                                    TextStyle style = new TextStyle(font,
                                            color, null);
                                    StyleRange keywordRange = new StyleRange(
                                            style);
                                    keywordRange.start = currentLength + index;
                                    keywordRange.length = keyword.getText()
                                            .length();

                                    ranges.add(keywordRange);
                                    // compare to see if this position is
                                    // already
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

                                    // only execute things if the same user
                                    // didn't
                                    // type it
                                    if (!myUser.equals(userId)) {
                                        executeSightsSounds(keyword);
                                    }
                                    // need to handle all instances of the
                                    // keyword
                                    // within the chat
                                    index = lowerCase.indexOf(text,
                                            text.length() + index);
                                }
                            }
                        }
                    }

                    styleAndAppendText(sb, offset, name, userId, ranges);
                }

                // Archive the message
                if (msgArchive != null) {
                    msgArchive.archive(sb.toString());
                }

                // Append the text to the search control.
                if (searchComp.isDisposed() == false) {
                    searchComp.appendText(sb.toString());
                }
            }
        });
    }

    /**
     * Determines if task bar notifications for new messages should be given in
     * this room (always for peer-to-peer, otherwise depends on preferences).
     * 
     * @return whether or not to notify the task bar
     */
    protected boolean shouldNotifyTaskbar() {
        return true;
    }

    protected abstract void styleAndAppendText(StringBuilder sb, int offset,
            String name, T userId, List<StyleRange> ranges);

    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, T userId, List<StyleRange> ranges, Color foreground,
            Color background) {
        StyleRange range = new StyleRange(messagesText.getCharCount(),
                sb.length(), foreground, null, SWT.NORMAL);
        // This must go first to be overridden by other ranges (name bolding,
        // alert words)
        ranges.add(0, range);

        range = new StyleRange(messagesText.getCharCount() + offset,
                (userId != null ? name.length() + 1 : sb.length() - offset),
                foreground, null, SWT.BOLD);
        ranges.add(range);
        messagesText.append(sb.toString());

        for (StyleRange newRange : ranges) {
            messagesText.setStyleRange(newRange);
        }

        int lineNumber = messagesText.getLineCount() - 1;
        messagesText.setLineBackground(lineNumber, 1, background);
        messagesText.setTopIndex(lineNumber);
    }

    /**
     * Find keys words in body of message starting at offset.
     * 
     * @param builder
     * @param offset
     * @return alertWords
     */
    protected List<AlertWord> retrieveAlertWords() {
        if (alertWords == null) {
            alertWords = CollaborationUtils.getAlertWords(false);
        }
        return alertWords;
    }

    /**
     * Place holder must override to do something.
     */
    protected void executeSightsSounds(AlertWord word) {
        String filename = word.getSoundPath();
        playSound(filename);
    }

    protected void playSound(String filename) {
        SoundUtil.playSound(filename);
    }

    protected String getJoinFile() {
        return Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.JOIN_FILE_FIELD_EDITOR_ID);
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
        setTitleImage(getImage(getSessionImageName()));
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
        if (messagesTextFont != null) {
            messagesTextFont.dispose();
        }

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
        composeTextViewer.getTextWidget().setFocus();
        /*
         * Revert image to normal (in case it was changed to notify of a message
         * being received)
         */
        setTitleImage(getImage(getSessionImageName()));
    }

    private void createNotifier(String id, String time, String body) {
        String titleText = "(" + time + ") " + id;
        PopupNotifier.notify(titleText, body);
    }

    @Subscribe
    public void changeFont(FontData data) {
        Font oldFont = messagesTextFont;
        messagesTextFont = new Font(Display.getCurrent(), data);
        messagesText.setFont(messagesTextFont);
        if (oldFont != null) {
            oldFont.dispose();
        }
    }

    public void setAlertWords(List<AlertWord> words) {
        alertWords = words;
    }

    protected abstract SessionMsgArchive createMessageArchive();

    /**
     * display formatted error message on chat window
     * 
     * @param sb
     *            builder containing message
     */
    public void sendErrorMessage(StringBuilder sb) {
        sendGenericMessage(sb, SWT.COLOR_RED);
    }

    /**
     * display formatted error message on chat window
     * 
     * @param sb
     *            builder containing message
     */
    protected void sendSystemMessage(StringBuilder sb) {
        sendGenericMessage(sb, SWT.COLOR_BLACK);
    }

    /**
     * display formatted error message on chat window
     * 
     * @param builder
     *            builder containing message
     * @param swtColor
     *            text color for message
     */
    private void sendGenericMessage(final StringBuilder builder,
            final int swtColor) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                Date date = new Date();
                String time = timeFormatter.get().format(date);
                builder.insert(0, "(" + time + ") : ");

                // Update the messagesText with the StyleRange highlights
                if (messagesText.isDisposed() == false) {
                    if (messagesText.getCharCount() != 0) {
                        builder.insert(0, "\n");
                    }

                    Color foreground = Display.getCurrent().getSystemColor(
                            swtColor);
                    Color background = Display.getCurrent().getSystemColor(
                            SWT.COLOR_WHITE);
                    StyleRange range = new StyleRange(messagesText
                            .getCharCount(), builder.length(), foreground,
                            null, SWT.BOLD);
                    List<StyleRange> ranges = new ArrayList<StyleRange>();
                    ranges.add(range);
                    styleAndAppendText(builder, 0, builder.toString(), null,
                            ranges, foreground, background);
                }

                // Archive the message
                if (msgArchive != null) {
                    msgArchive.archiveLine(builder.toString());
                }

                // Append the text to the search control.
                if (searchComp.isDisposed() == false) {
                    searchComp.appendText(builder.toString());
                }
            }
        });
    }

    /**
     * Get corresponding Color from map using RGB
     * 
     * @param rgb
     * @return
     */
    protected Color getColorFromRGB(RGB rgb) {
        Color color = colors.get(rgb);
        if (color == null) {
            color = new Color(Display.getCurrent(), rgb);
            colors.put(rgb, color);
        }
        return color;
    }

    /**
     * Determines if the given date (of the current message) is a newer day
     * compared to the stored date (of the last message) and replaces the stored
     * date with the given date.
     * 
     * @param currentMessageDay
     *            the date to compare with the stored date
     * @return true if the message with the given date occurs on a new day from
     *         the last message, false otherwise
     */
    private boolean storeAndCompareTimestamp(Date currentMessageDay) {
        boolean newDay = lastMessageDay == null
                || TimeUtil.isNewerDay(lastMessageDay, currentMessageDay,
                        TimeUtil.GMT_TIME_ZONE);
        this.lastMessageDay = currentMessageDay;
        return newDay;
    }
}
