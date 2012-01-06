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

import string

class StringUtils:
    def __init__(self):
        pass

    def sentence(self, str, addPeriod=1):
        "Make a sentence out of the string, s"
        str = string.strip(str)
        if str == "":
            return str

        # Remove leading and trailing spaces
        if addPeriod:
            period = ". "
        else:
            period = ""
        if len(str) == 1:
            return string.capitalize(str[0]) + period
        return string.capitalize(str[0]) + str[1:len(str)] + period

    def endline(self, phrase, linelength=66, breakStr=[" ", "..."]):
        "Insert endlines into phrase"

        # Break into sub-phrases separated by \n
        subPhrases = string.split(phrase,"\n")

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
        

##    def linebreak(self, phrase, linelength, breakStr=[" ", "..."]):
##        # Break phrase into lines the given linelength
##        start = 0
##        str = ""
##        further = 0
##        while start < len(phrase):
##            end = start + linelength + further
##            if end >= len(phrase):
##                str = str + phrase[start:len(phrase)] + "\n"
##                break
##            breakFound = 0
##            #search for break characters in string
##            for breakChars in breakStr:
##                ind = string.rfind(phrase, breakChars, start, end)
##                if ind >= 0:
##                    breakFound = 1
##                    break
##            #if not found, then we need to search further, this makes the
##            #line too long, but it is better than simply splitting a word
##            #in the middle of it.
##            if breakFound == 0:
##                further = further + 1
##                continue
                                                                                    
##            if breakChars != " ":
##                # We want to preserve the break characters, not drop them
##                includeInd = ind + len(breakChars)
##            else:
##                includeInd = ind

##            str = str + phrase[start:includeInd] + "\n"
##            start = ind + len(breakChars)
##            further = 0
##        return str
    

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

    def combinePhrases(self, phrases, separator="...", conjunction="..."):
        # Combine the list of phrases using separator and conjunction
        newPhrase = ""
        index = 0
        length = len(phrases)
        for phrase in phrases:
            newPhrase = newPhrase + phrase

            # if last one, do not add conjunction
            if index == length - 1: break

            # if second to last one use conjunction
            if index == length - 2:
                newPhrase = newPhrase + conjunction
            # otherwise
            else:
                newPhrase = newPhrase + separator
            index = index + 1

        newPhrase = self.sentence(newPhrase)
        newPhrase = string.strip(newPhrase)
        return newPhrase

    def labelIndent(self, phrase, descriptor):
        indentString = ""
        for i in range(len(descriptor)):
            i = i # for pychecker
            indentString = string.join([indentString," "], "")
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
        words = string.split(text, '\n')
        words = self.splitIntoWords(words, breakStrings)
        #print "split words", words
            
        # eliminate all new lines and create a list of words
        #words = string.split(text, '\n')
        #textData = string.join(words)        
        #words = string.split(textData)        
        if len(words) == 0:
            return ""
        #print "words", words

        # find out how many spaces the 1st line has been indented based on
        # the input text.
        additionalIndent = string.find(text, words[0])
        firstLineAdditionalIndent = ''
        for i in xrange(additionalIndent):
            i = i # for pychecker
            firstLineAdditionalIndent = firstLineAdditionalIndent + ' '

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
                strWords = string.split(word, breakStr)
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
        words = string.splitfields(str, " ")
        length = len(words)
        if words[length-1] == removeStr:
            return string.joinfields(words[0:length-1], " ")
        else:
            return str
        
    def addTextList(self, words, wordList, preposition=" with ", conjunction=" and "):
        # Add a list of text phrases to the given words using the
        # given preposition and conjunction.
        # For example:
        #   words = "Some thunderstorms may be severe"
        #   wordList = ["damaging winds", "hail"]
        #   Some thunderstorms may be severe with damaging winds and hail.        
        index = 0
        length = len(wordList)
        for wordStr in wordList:
            if index == 0:
                words = words + preposition + wordStr
            else:
                # if last one, use "and" instead of ","
                if index == length - 1:
                    words = words + conjunction + wordStr
                # otherwise
                else:
                    words = words + "..." + wordStr
            index = index + 1
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
            lowerCase = 0
        if not lowerCase:
            text = text.upper()  # convert to upper case
        return text
    
    def convertToLower(self, text, allLower=0):
        if allLower:
            return text.lower()
        try:
            lowerCase = self._lowerCase
        except:
            lowerCase = 0
        if lowerCase:
            words = text.split()
            new = []
            for word in words:
                new.append(word.capitalize())
            return string.join(new)
        else:
            return text
