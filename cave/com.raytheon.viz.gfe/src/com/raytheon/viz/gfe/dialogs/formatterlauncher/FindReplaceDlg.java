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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.EnumSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display the Find and/or Replace dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 25 APR 2011  8935       gzhou       Modify the findString function
 *                                     to find the string with meta character(s)
 * 02 MAY 2011  9221       gzhou       Modify the handleClickFind function
 *                                     to handle the BACKWARD_SEARCH option
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class FindReplaceDlg extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FindReplaceDlg.class);

    /**
     * Flag indicating if the dialog should display the replace controls.
     */
    private boolean findAndReplace = true;

    /**
     * StyledTextComp control upon which this Find/Replace dialog searches.
     */
    private StyledTextComp editorComp;

    /**
     * StyledText control upon which this Find/Replace dialog searches.
     */
    private StyledText editorST;

    /**
     * Find text control.
     */
    private Text findTF;

    /**
     * Replace text control.
     */
    private Text replaceTF;

    /**
     * Search forwards radio button.
     */
    private Button forwardsRdo;

    /**
     * Search backwards radio button.
     */
    private Button backwardsRdo;

    /**
     * Regular expression check box.
     */
    private Button regExpChk;

    /**
     * Exact match check box.
     */
    private Button exactChk;

    /**
     * Ignore case check box.
     */
    private Button noCaseChk;

    /**
     * Start position radio button.
     */
    private Button startRdo;

    /**
     * End position radio button.
     */
    private Button endRdo;

    /**
     * Cursor position radio button.
     */
    private Button cursorRdo;

    /**
     * Enumerator that encapsulates search dialog options.
     */
    private enum FindReplaceOptions {
        FORWARD_SEARCH, BACKWARD_SEARCH, REGEX_SEARCH, EXACT_MATCH, IGNORE_CASE, SEARCH_POS_START, SEARCH_POS_END, SEARCH_POS_CURSOR
    };

    /**
     * The list of search options that have been selected by the user.
     */
    private Set<FindReplaceOptions> searchOptions;

    /**
     * A flag that determines whether or not the user has clicked the Replace
     * button two times. In AWIPS I, the first click selects the search term and
     * the second click makes the replacement.
     */
    private boolean readyToReplace = false;

    /**
     * The location of the cursor in the text field when the user asked to
     * perform a search bound by the location of the cursor.
     */
    private int cursorOffset = 0;

    /**
     * The location of the match of the search term when doing a replace. The x
     * field will contain the starting offset of the match and the y field will
     * contain the length of the string.
     */
    private Point selectionToBeReplaced;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param findAndReplace
     *            Flag indicating if the replace controls should be displayed.
     * @param editorComp
     *            Text that will be searched against.
     */
    public FindReplaceDlg(Shell parent, boolean findAndReplace,
            StyledTextComp editorComp) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);

        this.findAndReplace = findAndReplace;
        this.editorComp = editorComp;
        this.editorST = editorComp.getTextEditorST();
        this.searchOptions = EnumSet.noneOf(FindReplaceOptions.class);
    }

    @Override
    protected void initializeComponents(Shell shell) {

        if (findAndReplace == true) {
            shell.setText("Find & Replace");
        } else {
            shell.setText("Find");
        }

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        createMainControls();
        createBottomButtons();
    }

    /**
     * Create the main controls on the display.
     */
    private void createMainControls() {
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        mainComp.setLayout(new GridLayout(1, false));

        createFindTextControl(mainComp);
        createDirectionControls(mainComp);
        createSearchOptions(mainComp);
        createPositionControls(mainComp);

        // If the replace text controls should be displayed then add
        // them to the display.
        if (findAndReplace == true) {
            createReplaceTextControl(mainComp);
        }
    }

    /**
     * Add the Find text control to the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createFindTextControl(Composite mainComp) {
        Composite findComp = new Composite(mainComp, SWT.NONE);
        findComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        findComp.setLayout(new GridLayout(2, false));

        Label findLbl = new Label(findComp, SWT.NONE);
        findLbl.setText("Find: ");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 300;
        findTF = new Text(findComp, SWT.BORDER);
        findTF.setLayoutData(gd);
        findTF.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                readyToReplace = false;
            }
        });
    }

    /**
     * Add the 'Find' option controls to the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createDirectionControls(Composite mainComp) {
        Group dirGroup = new Group(mainComp, SWT.NONE);
        dirGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        dirGroup.setLayout(new GridLayout(2, false));
        dirGroup.setText(" Search Direction: ");

        forwardsRdo = new Button(dirGroup, SWT.RADIO);
        forwardsRdo.setText("Forwards");
        forwardsRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!searchOptions.contains(FindReplaceOptions.FORWARD_SEARCH)) {
                    searchOptions.add(FindReplaceOptions.FORWARD_SEARCH);
                    searchOptions.remove(FindReplaceOptions.BACKWARD_SEARCH);
                    readyToReplace = false;
                }
            }
        });
        forwardsRdo.setSelection(true);
        searchOptions.add(FindReplaceOptions.FORWARD_SEARCH);

        backwardsRdo = new Button(dirGroup, SWT.RADIO);
        backwardsRdo.setText("Backwards");
        backwardsRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!searchOptions.contains(FindReplaceOptions.BACKWARD_SEARCH)) {
                    searchOptions.remove(FindReplaceOptions.FORWARD_SEARCH);
                    searchOptions.add(FindReplaceOptions.BACKWARD_SEARCH);
                    readyToReplace = false;
                }
            }
        });
    }

    /**
     * Add the search options to the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createSearchOptions(Composite mainComp) {
        Group optionsGroup = new Group(mainComp, SWT.NONE);
        optionsGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        optionsGroup.setLayout(new GridLayout(3, false));
        optionsGroup.setText(" Search Options: ");

        regExpChk = new Button(optionsGroup, SWT.CHECK);
        regExpChk.setText("Regular Expression");
        regExpChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (regExpChk.getSelection()) {
                    searchOptions.add(FindReplaceOptions.REGEX_SEARCH);
                } else {
                    searchOptions.remove(FindReplaceOptions.REGEX_SEARCH);
                }

                readyToReplace = false;
            }
        });

        exactChk = new Button(optionsGroup, SWT.CHECK);
        exactChk.setText("Exact Match");
        exactChk.setSelection(true);
        searchOptions.add(FindReplaceOptions.EXACT_MATCH);
        exactChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (exactChk.getSelection()) {
                    searchOptions.add(FindReplaceOptions.EXACT_MATCH);
                } else {
                    searchOptions.remove(FindReplaceOptions.EXACT_MATCH);
                }

                readyToReplace = false;
            }
        });

        noCaseChk = new Button(optionsGroup, SWT.CHECK);
        noCaseChk.setText("Ignore Case");
        noCaseChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (noCaseChk.getSelection()) {
                    searchOptions.add(FindReplaceOptions.IGNORE_CASE);
                } else {
                    searchOptions.remove(FindReplaceOptions.IGNORE_CASE);
                }

                readyToReplace = false;
            }
        });
    }

    /**
     * Add the position controls to the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createPositionControls(Composite mainComp) {
        Group positionGroup = new Group(mainComp, SWT.NONE);
        positionGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        positionGroup.setLayout(new GridLayout(3, false));
        positionGroup.setText(" Search Position: ");

        startRdo = new Button(positionGroup, SWT.RADIO);
        startRdo.setText("Start");
        startRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!searchOptions
                        .contains(FindReplaceOptions.SEARCH_POS_START)) {
                    searchOptions.add(FindReplaceOptions.SEARCH_POS_START);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_END);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_CURSOR);

                    readyToReplace = false;
                }
            }
        });

        endRdo = new Button(positionGroup, SWT.RADIO);
        endRdo.setText("End");
        endRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!searchOptions.contains(FindReplaceOptions.SEARCH_POS_END)) {
                    searchOptions.add(FindReplaceOptions.SEARCH_POS_END);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_START);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_CURSOR);

                    readyToReplace = false;
                }
            }
        });

        cursorRdo = new Button(positionGroup, SWT.RADIO);
        cursorRdo.setText("Cursor");
        cursorRdo.setSelection(true);
        searchOptions.add(FindReplaceOptions.SEARCH_POS_CURSOR);
        cursorRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!searchOptions
                        .contains(FindReplaceOptions.SEARCH_POS_CURSOR)) {
                    searchOptions.add(FindReplaceOptions.SEARCH_POS_CURSOR);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_END);
                    searchOptions.remove(FindReplaceOptions.SEARCH_POS_START);
                    cursorOffset = editorST.getCaretOffset();

                    readyToReplace = false;
                }
            }
        });
    }

    /**
     * Add the replace text control to the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createReplaceTextControl(Composite mainComp) {
        Composite replaceComp = new Composite(mainComp, SWT.NONE);
        replaceComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        replaceComp.setLayout(new GridLayout(2, false));

        Label replaceLbl = new Label(replaceComp, SWT.NONE);
        replaceLbl.setText("Replace: ");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        replaceTF = new Text(replaceComp, SWT.BORDER);
        replaceTF.setLayoutData(gd);
        replaceTF.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                readyToReplace = false;
            }
        });
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(4, false));

        gd = new GridData(120, SWT.DEFAULT);
        Button findBtn = new Button(buttons, SWT.PUSH);
        findBtn.setText("Find");
        findBtn.setLayoutData(gd);
        findBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleClickFind(event);
            }
        });

        // With the replace and replace all feature, the style ranges will
        // automatically prevent us from replacing protected (blue) text so we
        // can just attempt the replacement and if nothing is done that is
        // acceptable.
        if (findAndReplace == true) {
            gd = new GridData(120, SWT.DEFAULT);
            Button replaceBtn = new Button(buttons, SWT.PUSH);
            replaceBtn.setText("Replace");
            replaceBtn.setLayoutData(gd);
            replaceBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    handleClickReplace(event);
                }
            });

            gd = new GridData(120, SWT.DEFAULT);
            Button replaceAllBtn = new Button(buttons, SWT.PUSH);
            replaceAllBtn.setText("Replace All");
            replaceAllBtn.setLayoutData(gd);
            replaceAllBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    handleClickReplaceAll(event);
                }
            });
        }

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }

    /**
     * Selection handler for the Replace All button. Will take a specified
     * search string and replace all occurrences of that string with the
     * specified replacement, unless it appears in a protected text block.
     * 
     * @param event
     *            The SelectionEvent object generated by the click event.
     * 
     */
    private void handleClickReplaceAll(SelectionEvent event) {
        if (!findTF.getText().equals("")) {
            String searchString = findTF.getText();
            String replaceString = replaceTF.getText();
            boolean replacedText = false;

            // AWIPS-I completely ignores the UI-selected options (except for
            // case, regex, and ignore case) when performing a Replace All. So
            // we will save off the current options, but use the normal
            // forward-based search to perform the replacements.
            EnumSet<FindReplaceOptions> uiSelections = EnumSet
                    .copyOf(this.searchOptions);
            this.searchOptions.add(FindReplaceOptions.FORWARD_SEARCH);
            this.searchOptions.add(FindReplaceOptions.SEARCH_POS_START);
            this.searchOptions.remove(FindReplaceOptions.BACKWARD_SEARCH);
            this.searchOptions.remove(FindReplaceOptions.SEARCH_POS_CURSOR);
            this.searchOptions.remove(FindReplaceOptions.SEARCH_POS_END);

            int[] matchRange = findString(searchString);
            while (!(matchRange[0] == 0 && matchRange[1] == 0)) {
                int newOffset = matchRange[1];

                editorST.replaceTextRange(matchRange[0], searchString.length(),
                        replaceString);

                if (editorST
                        .getTextRange(matchRange[0], replaceString.length())
                        .equals(replaceString)) {
                    newOffset = matchRange[0] + replaceString.length();
                    replacedText = true;
                }

                if (newOffset >= editorST.getText().length()) {
                    break;
                }

                matchRange = findString(searchString, newOffset);
            }

            this.searchOptions = uiSelections;
            if (replacedText) {
                editorComp.reParse();
            }
        }

        readyToReplace = false;
        cursorOffset = 0;
    }

    /**
     * Selection event handler for the Replace button. Will take a specified
     * search string and replace the first occurrence of that string with the
     * specified replacement, unless it appears in a protected text block. As in
     * AWIPS I, this is a two stage process. First, the user will click this
     * button, and the search string will be selected. Clicking on this button a
     * second time will perform the replacement and select the changed area. Any
     * changes to the find or replace string fields or any changes to the
     * selections on the dialog will result in resetting the readyToReplace
     * flag.
     * 
     * @param event
     *            The SelectionEvent object generated by the click event.
     * 
     */
    private void handleClickReplace(SelectionEvent event) {
        String searchString = findTF.getText();

        if (!searchString.equals("")) {
            if (readyToReplace
                    && selectionToBeReplaced.equals(editorST
                            .getSelectionRange())) {
                String replaceString = replaceTF.getText();

                editorST.replaceTextRange(selectionToBeReplaced.x,
                        searchString.length(), replaceString);

                int[] replaceMatch = findString(replaceString,
                        selectionToBeReplaced.x);
                if (!(replaceMatch[0] == 0 && replaceMatch[1] == 0)) {
                    editorComp.reParse();
                    editorST.setCaretOffset(replaceMatch[1]);
                }
            }

            readyToReplace = false;

            cursorOffset = editorST.getCaretOffset();
            readyToReplace = findAndSelectString(searchString);
            selectionToBeReplaced = editorST.getSelectionRange();
        }
    }

    /**
     * Selection event handler for the Find button. Will take a specified search
     * string and select the first occurrence of that string.
     * 
     * @param event
     *            The SelectionEvent object generated by the click event.
     * 
     */
    private void handleClickFind(SelectionEvent event) {
        if (findTF.getText().equals(""))
            return;

        cursorOffset = editorST.getCaretOffset();

        // #9221
        // For BACKWARD_SEARCH option:
        // Need skip the previously found string
        if (searchOptions.contains(FindReplaceOptions.BACKWARD_SEARCH)) {
            if (editorST.getSelectionRange() != null
                    && editorST.getSelectionRange().y > 0) {
                cursorOffset -= editorST.getSelectionRange().y;
                if (cursorOffset < 0) {
                    cursorOffset = 0;
                }
            }
        }

        // Need update the variables for the replace function so that
        // User can replace string with one click of the replace button.

        readyToReplace = findAndSelectString(findTF.getText());
        selectionToBeReplaced = editorST.getSelectionRange();

    }

    /**
     * Searches the StyledText control for the first occurrence of the provided
     * string and selects the string if it is found. Search will be controlled
     * by the GUI options selected.
     * 
     * @param searchString
     *            The string that will be searched for in the StyledText
     *            control.
     * @return Returns a boolean indicating whether or not the provided string
     *         could be found.
     * 
     */
    private boolean findAndSelectString(String searchString) {
        int[] matchRange = findString(searchString);

        if (!(matchRange[0] == 0 && matchRange[1] == 0)) {
            editorST.setSelection(matchRange[0], matchRange[1]);
            editorST.showSelection();
            cursorOffset = editorST.getCaretOffset();

            return true;
        }

        return false;
    }

    /**
     * Searches the StyledText control for the first occurrence of the provided
     * string bound by the provided starting and stopping indices. Search will
     * be bound by the provided bounds and controlled by the GUI options
     * selected.
     * 
     * @param searchString
     *            The string that will be searched for in the StyledText
     *            control.
     * @param startIndex
     *            The lower bound of the search range. In a forwards search,
     *            this is the "starting" point of the search and, in a backwards
     *            search, this is the "ending" point of the search.
     * @param endIndex
     *            The upper bound of the search range. In a forwards search,
     *            this is the "ending" point of the search and, in a backwards
     *            search, this is the "starting" point of the search.
     * @return A two-element integer array containing the beginning and ending
     *         indices of the matched phrase. If the string could not be found,
     *         {0, 0} is returned.
     */

    /*
     * The original source code does not work for string with meta character(s).
     * Changed to use the Pattern.LITERAL flag to find meta character(s).
     * 
     * Hint:
     * 
     * 1) The combination "\\b\\QWordWithMetaCharacter\\E\\b" does not work.
     * 
     * 2) The "Exact Match" does not have effect the same as AWIPS I. Hope the
     * Pattern class will have new option for whole word only in the future.
     * 
     * --gzhou 21-04-2011
     */
    private int[] findString(String searchString, int startIndex, int endIndex) {
        int regexFlags = 0;
        String searchRegex = searchString;
        int[] match = { 0, 0 };

        if (!searchOptions.contains(FindReplaceOptions.REGEX_SEARCH)) {
            // searchRegex = "\\Q" + searchRegex + "\\E";

            // set the regex flag to literal when the "Regular Expression"
            // option is toggled off so that the meta character will be
            // searched the same as other regular characters.

            regexFlags |= Pattern.LITERAL;
        }

        // In AWIPS I, it appears that when the "Regular Expression" search
        // option is selected, the "Exact Match" option has no effect.
        // Replicating that behavior here...
        //
        // TODO: There is a exact match behavior that I could not replicate with
        // Java's regex functionality--performing exact match searches for
        // strings starting or ending with punctuation will fail. For example,
        // searching for ".TODAY..." in the string ".TODAY...\n.TOMORROW...\n"
        // will fail.
        // if (searchOptions.contains(FindReplaceOptions.EXACT_MATCH)
        // && !searchOptions.contains(FindReplaceOptions.REGEX_SEARCH))
        // searchRegex = "\\b" + searchRegex + "\\b";
        if (searchOptions.contains(FindReplaceOptions.IGNORE_CASE))
            regexFlags |= Pattern.CASE_INSENSITIVE;

        // Need handle the PatternSyntaxException when user searches meta
        // character(s) but forgot to toggle on the regex option.

        try {
            Pattern searchPattern = Pattern.compile(searchRegex, regexFlags);
            Matcher matcher = searchPattern.matcher(editorST.getText());

            matcher = matcher.region(startIndex, endIndex);

            while (!matcher.hitEnd()) {
                if (matcher.find()) {
                    match[0] = matcher.start();
                    match[1] = matcher.end();
                    if (searchOptions
                            .contains(FindReplaceOptions.FORWARD_SEARCH))
                        break;
                }
            }
        } catch (PatternSyntaxException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to parse the search string", e);
            match[0] = match[1] = 0;
        }

        return match;
    }

    /**
     * Searches the StyledText control for the first occurrence of the provided
     * string. Search will be controlled by the GUI options selected.
     * 
     * @param searchString
     *            The string that will be searched for in the StyledText
     *            control.
     * @return A two-element integer array containing the beginning and ending
     *         indices of the matched phrase. If the string could not be found,
     *         {0, 0} is returned.
     */
    private int[] findString(String searchString) {
        int searchStartIndex;
        int searchEndIndex;

        if (searchOptions.contains(FindReplaceOptions.FORWARD_SEARCH)) {
            searchStartIndex = (searchOptions
                    .contains(FindReplaceOptions.SEARCH_POS_START) ? 0
                    : (searchOptions
                            .contains(FindReplaceOptions.SEARCH_POS_END) ? editorST
                            .getText().length() - 1 : cursorOffset));
            searchEndIndex = editorST.getText().length() - 1;
        } else {
            searchStartIndex = 0;
            searchEndIndex = (searchOptions
                    .contains(FindReplaceOptions.SEARCH_POS_START) ? 0
                    : (searchOptions
                            .contains(FindReplaceOptions.SEARCH_POS_END) ? editorST
                            .getText().length() - 1 : cursorOffset));
        }

        return findString(searchString, searchStartIndex, searchEndIndex);
    }

    /**
     * Searches the StyledText control for the first occurrence of the provided
     * string. Search will be be bound by the provided lower bound and
     * controlled by the GUI options selected.
     * 
     * @param searchString
     *            The string that will be searched for in the StyledText
     *            control.
     * @param startIndex
     *            The lower bound of the search range. In a forwards search,
     *            this is the "starting" point of the search and, in a backwards
     *            search, this is the "ending" point of the search.
     * @return A two-element integer array containing the beginning and ending
     *         indices of the matched phrase. If the string could not be found,
     *         {0, 0} is returned.
     */
    private int[] findString(String searchString, int startIndex) {
        int searchStartIndex;
        int searchEndIndex;
        int[] match = { 0, 0 };

        if (searchString == null || searchString.equals("")) {
            return match;
        }

        if (searchOptions.contains(FindReplaceOptions.FORWARD_SEARCH)) {
            searchStartIndex = startIndex;
            searchEndIndex = editorST.getText().length() - 1;
        } else {
            searchStartIndex = startIndex;
            searchEndIndex = (searchOptions
                    .contains(FindReplaceOptions.SEARCH_POS_START) ? 0
                    : (searchOptions
                            .contains(FindReplaceOptions.SEARCH_POS_END) ? editorST
                            .getText().length() - 1 : cursorOffset));
        }

        return findString(searchString, searchStartIndex, searchEndIndex);
    }
}
