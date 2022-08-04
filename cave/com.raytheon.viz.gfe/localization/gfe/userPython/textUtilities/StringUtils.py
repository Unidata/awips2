##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StringUtils.py
# Methods for manipulating strings such as sentences, phrases, indentations.
#
# Author: hansen
# ----------------------------------------------------------------------------
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/22/2015       4027         randerso       Changed lowerCase default to True
#    07/15/2016       5749         randerso       Added punctuateList method
#    10/27/2016       5749         randerso       Changed combinePhrases to use commas
#    11/28/2016       5749         randerso       Changed addTextList to use commas
#    03/15/2020       DR21821      NFTF           Remove unneccesary comma in Wx attributes
#
##

##
# This is a base file that is not intended to be overridden.
##

import re

class StringUtils:
    def __init__(self):
        pass

    def sentence(self, s, addPeriod=1):
        "Make a sentence out of the string, s"
        
        # if string is entirely whitespace return empty string
        if re.match(r'^\s*$', s) is not None:
            return ""

        # Remove leading and trailing spaces
        if addPeriod:
            period = ". "
        else:
            period = ""

        return s[0].upper() + s[1:] + period

    def endline(self, phrase, linelength=66, breakStr=[" ", "..."]):
        "Insert endlines into phrase"

        # Break into sub-phrases separated by \n
        subPhrases = phrase.split("\n")

        # Break each sub-phrase into lines
        str = ""
        for subPhrase in subPhrases:
            if subPhrase == "":
                str = str + "\n"
            else:
                str = str + self.linebreak(subPhrase, linelength, breakStr)
        return str

##   Contribution from Jay Smith TK 3268.  Old version follows.
##        I discovered what I consider a bug in the linebreak method of the
##        StringUtils module. Let's say you pass in breakStr=['...', ' ']. The
##        method will never break on ' ' if '...' is present in the string being
##        tested. I believe the linebreak method should check each element of
##        breakStr, determine which element appears farthest to the right in the
##        string, and use that element as the break.
##        I've attached a modified linebreak method which does what I've outlined.
##        Additionally, my linebreak method will not let a line end with a number.
##        This is to prevent a number and its units from appearing on different
##        lines.

##    Restructured by Hansen TK to add forceBreakStr capability when no breakStr
##    is found in a given linelength of characters.
    def linebreak(self, phrase, linelength, breakStr=[' ', '...'],
                  forceBreakStr=[" ","/"]):
        # Break phrase into lines of the given linelength
        # Prevents a line break on a number.
        # If no breakStr is found for a given linelength of characters,
        # force a break on the rightmost forceBreakStr.
        text = ''
        start = 0
        end = start + linelength
        subPhrase = phrase[start:end]
        while len(subPhrase) == linelength:
            maxIndex, breakChars = self.findRightMost(subPhrase, breakStr)
            if maxIndex == -1:
                # Didn't find any breakStr; line is too long.
                # Find the rightmost force break string, if possible.
                forceIndex, breakChars = self.findRightMost(subPhrase, forceBreakStr)
                if forceIndex == 0:
                    # space in first position: will be skipped.
                    pass
                elif forceIndex > 0:
                    subPhrase = subPhrase[0:forceIndex]
                    text = '%s%s\n' % (text, subPhrase)
                    start += forceIndex
                else:
                    # no forcebreak spot, either.
                    # break at linelength.
                    text = '%s%s\n' % (text, subPhrase)
                    start += linelength
            elif maxIndex == 0:
                pass # space in first position: will be skipped
            else:
                text = '%s%s\n' % (text, subPhrase[:maxIndex])
                start += maxIndex
            if breakChars == " ":
                # Skip the space
                start +=1
            end = start + linelength
            subPhrase = phrase[start:end]
        if subPhrase:
            return '%s%s\n' % (text, subPhrase)
        else:
            # It's possible for subPhrase to be [] coming out of the while
            # loop. In that case, we just need to return text.
            return text

    def findRightMost(self, text, breakStr=[" "], nonNumeric=1):
        # Return the index of the right most break string characters
        # and the break characters that were found.
        # If nonNumeric, then make sure the index does not refer to
        # a numeric character.
        # If the break characters are a space, the index indicate
        # the character prior to the space.
        maxIndex = -1
        maxChars = ''
        for breakChars in breakStr:
            index = text.rfind(breakChars)
            done = False
            while index > 0 and not done:
                # Check for a numeric at end of line
                if nonNumeric and breakChars == " " and text[index-1].isdigit():
                    # Try to find the next right most break char
                    index = text.rfind(breakChars, 0, index-1)
                    continue
                done = True
            if index > maxIndex:
                maxIndex = index
                maxChars = breakChars
        if maxIndex == -1:
            return maxIndex, maxChars
        if maxChars == ' ':
            index = maxIndex
        else:
            # We want to keep the breakChars, which are assumed not to end
            # with a number
            index = maxIndex + len(maxChars)
        return index, maxChars

    def combineSentences(self, textStr):
        # Given a string of sentences, combine consecutive single word
        # sentences, e.g.  Warm. Dry. --> Warm...Dry.
        if textStr == '':
            return ''
        newTextStr = ''
        # Split the string into sentences
        sentences = textStr.split('.')
        singleWords = []
        for sent in sentences[:-1]:
            # See if sentence consists of a single word
            words = sent.split() 
            # If single word, append it to list
            if len(words) == 1:
                singleWords.append(words[0].lower())
            # Otherwise, make sentence of any previous single words
            # and add untouched sentence to new string
            else:
                if len(singleWords) > 0:
                    if newTextStr != '':
                        newTextStr = newTextStr + ' '
                    newTextStr = newTextStr + self.combinePhrases(singleWords)
                singleWords = []
                newTextStr = newTextStr + sent + '.'
        # Clear out remaining single words
        if newTextStr[:-1] != ' ': 
            newTextStr = newTextStr + ' ' 
        if len(singleWords) > 0:
            newTextStr = newTextStr + self.combinePhrases(singleWords)
        newTextStr = newTextStr.replace('... ', '...')
        return newTextStr

    def combinePhrases(self, phrases, separator=", ", conjunction=", "):
        # Combine the list of phrases using separator and conjunction
        newPhrase = ""
        index = 0
        length = len(phrases)
        for phrase in phrases:
            newPhrase = newPhrase + phrase

            # if last one, do not add conjunction
            if index == length - 1:
                break

            # if second to last one use conjunction
            if index == length - 2:
                newPhrase = newPhrase + conjunction
            # otherwise
            else:
                newPhrase = newPhrase + separator
            index = index + 1

        newPhrase = self.sentence(newPhrase)
        newPhrase = newPhrase.strip()
        return newPhrase

    def labelIndent(self, phrase, descriptor):
        indentString = " " * len(descriptor)
        result = self.indentText(descriptor + phrase, "", indentString)
        #print descriptor, indentString, len(descriptor), len(indentString)
        #print result, "\n"
        return result

    def indentText(self, text, indentFirstString = '', indentNextString = '',
                   maxWidth=69, breakStrings=[" "]):
        # indentText returns a formatted string which is at most maxWidth
        # columns in width, with the first line indented by "indentFirstString"
        # and subsequent lines indented by indentNextString.  Any leading spaces
        # in the first line are preserved.


        out = ''   # total output
        line = ''  # each line

        #print "text before", text
        # eliminate all new lines and create a list of words
        words = text.split('\n')
        words = self.splitIntoWords(words, breakStrings)
        #print "split words", words
            
        # eliminate all new lines and create a list of words
        #words = text.split('\n')
        #textData = " ".join(words)        
        #words = textData.split()        
        if len(words) == 0:
            return ""
        #print "words", words

        # find out how many spaces the 1st line has been indented based on
        # the input text.
        additionalIndent = text.find(words[0])
        firstLineAdditionalIndent = ' ' * additionalIndent

        # now start assembling the output
        line = line + indentFirstString + firstLineAdditionalIndent
        additional = indentFirstString + firstLineAdditionalIndent
        for w in words:
            if len(line) + len(w) + 1 > maxWidth:
                out = out + line + "\n"
                line = indentNextString + w
            else:
                if len(out) == 0 and len(line) == len(additional):
                    line = line + w   #first line, don't add a space
                else:
                    #line = line + ' ' + w   #subsequent words, add a space
                    line = line +  w   #subsequent words, add a space
        if len(line):
            out = out + line

        #print "text after", out + "\n"
        return out + "\n"

    def splitIntoWords(self, words, breakStrings=[" "]):
        # Break the list of words further
        # using the list of breakStrings.
        for breakStr in breakStrings:
            newWords = []
            # Break each word on the breakStr
            for word in words:
                # Split the word with breakStr
                strWords = word.split(breakStr)
                if len(strWords) > 1:
                    newStrWords = []
                    # Add the breakStr back in except for last one
                    index = 0
                    length = len(strWords)-1
                    for strWord in strWords:
                        if strWord == "":
                            continue
                        if index < length:
                            strWord += breakStr
                        newStrWords.append(strWord)
                        index += 1
                    strWords = newStrWords
                # Add these words to the new words list
                newWords = newWords + strWords
            words = newWords
        return words
    
    def removeLast(self, str, removeStr):
        # If the str ends in removeStr, remove it
        # For example,
        #   str = rain and
        #   removeStr = and
        #   return  rain
        str = str.rstrip()
        removeStr = removeStr.strip()
        words = str.split(" ")
        if words[-1] == removeStr:
            return " ".join(words[0:-1])
        else:
            return str
        
    def addTextList(self, words, wordList, preposition=" with ", conjunction=" and ", oxford=True):
        # Add a list of text phrases to the given words using the
        # given preposition and conjunction.
        # For example:
        #   words = "Some thunderstorms may be severe"
        #   wordList = ["damaging winds", "hail"]
        #   Some thunderstorms may be severe with damaging winds and hail.

        length = len(wordList)
        if length > 0:
            words += preposition

            # add comma separated list of all but final word
            words += ", ".join(wordList[0:-1])

            # add oxford comma if desired
            if length > 2 and oxford:
                words += ","

            # add conjunction if needed
            if length > 1:
                words += conjunction

            # add final word
            words += wordList[-1]

        return words

    def addSpace(self, str, place="trailing"):
        # Add a trailing space to str
        #  IF it is non-empty and the last character is not a trailing space
        if str is None:
            return ""
        if str != "" and str[-1:] != " ":
            if place == "trailing":
                str = str + " "
            elif place == "leading":
                str = " " + str
        return str

    def removeSuffixes(self, names, suffix):
        newNames = []
        sIndex = -len(suffix)
        #print "\nRemoving", suffix, names
        for name in names:
            if name[sIndex:] == suffix:
                newNames.append(name.rstrip(suffix))
            else:
                newNames.append(name)
        #print "Returning", newNames
        return newNames

    def convertToUpper(self, text):
        try:
            lowerCase = self._lowerCase
        except:
            lowerCase = True
        if not lowerCase:
            text = text.upper()  # convert to upper case
        return text
    
    def convertToLower(self, text, allLower=0):
        if allLower:
            return text.lower()
        try:
            lowerCase = self._lowerCase
        except:
            lowerCase = True
        if lowerCase:
            words = text.split()
            new = []
            for word in words:
                new.append(word.capitalize())
            return ' '.join(new)
        else:
            return text

    def replaceLast(self, str, str1, str2):
        """ Replace the last occurrence of str1 in str with str2
        """
        
        return str2.join(str.rsplit(str1,1))

    def punctuateList(self, items):
        """ Joins a list of strings into a comma separated list using 
            the Oxford comma if more than 3 items in the list
        """ 
        
        s = ", ".join(items)
        
        if len(items) > 2:
            s = self.replaceLast(s, ", ", ", and ")
        elif len(items) == 2:
            s = self.replaceLast(s, ", ", " and ")
        
        return s
