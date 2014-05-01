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
import os
import types
import xml.etree.ElementTree as ET
import binascii as BA
import lib.Util as util

##############################################################################
# Class that manages canonical XML messages. The idea here
# is to support building of messages and to support extracting
# of data from the XML. The XML format used here is
#
#   <message>
#      <header>
#         <property name="..." value="..."/>
#      </header>
#      <body>
#         <responses>
#            <...>
#         </responses>
#         <![CDATA[...]]>
#      </body>
#      <![CDATA[...]]>
#   </message>
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1584          mfegan         Initial Creation.
##############################################################################
class Message:
    # initializer.
    def __init__(self,bcode=False):
        self.xml = ""
        self.tree = None
        self.bcode = bcode

    def __str__(self):
        return 'Message[' + self.xml + ']'
    # Clears the object.
    def clear(self):
        self.xml = ""
        self.tree = None

    # Sets the XML string to parse into the message
    def setXML(self,xml):
        self.xml = xml

    # returns the XML string associated with the message
    def getXML(self):
        return self.xml

    # Parses an XML string. Will parse either the (optional) XML
    # string passed in or the previously set XML string.
    #
    # args:
    #   xml:   (optional) the XML string to parse
    # return:
    #   void
    # raises:
    #    MessageError when any error occurs
    def parse(self,xml=None):
        if xml == None:
            xml = self.xml
        try:
            self.tree = ET.fromstring(xml)
            self.xml = ET.tostring(self.tree)
        except Exception , e:
            raise MessageError("Unable to parse xml", e)

    # Returns the entire message as an ElementTree.
    #
    # raises:
    #    MessageError when any error occurs
    def getMessage(self):
        try:
            return self.tree
        except Exception,e:
            raise MessageError("Unable to retrieve message", e)

    # Determines if the message body contains a response of the specified type.
    #
    # args:
    #   type: name of the attribute to check
    # return:
    #   True if the element has the specified type, False otherwise
    # raises:
    #    MessageError when any error occurs
    def checkResponseType(self,type):
        path = 'body/responses'
        try:
            element = self.tree.find(path)
            if element == None:
                return False
            for attr in element.keys():
                if attr.endswith('type'):
                    return (element.get(attr,None) == type)
            return False
        except Exception,e:
            raise MessageError("Unable to access element at " + path, e)

    # Returns the response(s) that is embedded in the body. The response
    # returned free of the enclosing XML.
    #
    # args:
    #    tag:    True if the response is in a nested tag, false otherwise.
    #    name:   name of the sub-element containing the desired response.
    #    incTag: (optional) True to return tag and contents
    # return:
    #    a list containing the responses.
    # raises:
    #    MessageError when any error occurs
    def getResponse(self,tag,name,incTag=False):
        path = 'body/responses'
        try:
            if tag:
                if incTag:
                    responses = self.tree.findall(path)
                    retVal = []
                    for resp in responses:
                        content = resp.find(name)
                        value = ET.tostring(content)
                        retVal.append(value.strip())
                    return retVal
                else:
                    path = path + '/' + name
                    return self.__getContents(path)
            else:
                return self.__getAttributes(path, name)
        except Exception,e:
            type = 'attribute=' + name
            if tag:
                type = 'tag=' + name
            raise MessageError("Unable to get response for " + type, e)

    # Helper method that extracts the contents of the tag with the
    # specified path. The contents are returned as a list of strings.
    # 
    # args:
    #   path: path to the tag of interest
    # return:
    #   list containing contents of all instances of the tag
    #   will be empty if the tag is not found
    # raises:
    #    MessageError when any error occurs
    def __getContents(self,path):
        try:
            if self.__checkText(path):
                retVal = []
                elements = self.tree.findall(path)
                for element in elements:
                    retVal.append(element.text)
                return retVal
            else:
                tags = self.tree.findall(path)
                retVal = []
                for tag in tags:
                    parts = []
                    for child in tag.getchildren():
                        parts.append(ET.tostring(child))
                    if len(parts) != 0:
                        retVal.append(''.join(parts))
                if len(retVal) == 0:
                    retVal = None
                return retVal
        except Exception,e:
            raise MessageError("Unable to get contents for " + path, e)

    # Gets the attributes for an XML attribute.
    #
    # args:
    #   path: the XML path to the desired element
    #   name: name of the attribute to retrieve
    def __getAttributes(self,path,name):
        try:
            elements = self.tree.findall(path)
            retVal = []
            for element in elements:
                value = element.get(name,None)
                if value is not None:
                    retVal.append(value)
            if len(retVal) == 0:
                retVal = None
            return retVal
        except Exception,e:
            raise MessageError("Unable to retrieve attributes for " + path + ", attr=" + name, e)

    # Determines if the specified element has text
    #
    # args:
    #   path: the XML path to the desired element
    # return:
    #   True:  if the specified element has text
    #   False: otherwise
    # raises:
    #    MessageError when any error occurs
    def __checkText(self,path):
        try:
            element = self.tree.find(path)
            return (not util.isEmptyString(element.text,True))
        except Exception,e:
            raise MessageError("Unable to access element " + path, e)

    # retrieves the header of the previously parsed XML message.
    #
    # return:
    #    Element representing the message header
    # raises:
    #    MessageError when any error occurs
    def getHeader(self):
        try:
            return self.tree.find('header')
        except Exception,e:
            raise MessageError("Unable to retrieve message header", e)

    # retrieves the header of the previously parsed XML message as a string.
    #
    # return:
    #    string containing the message header
    # raises:
    #    MessageError when any error occurs
    def getStrHeader(self):
        try:
            return ET.tostring(self.tree.find('header'))
        except Exception,e:
            raise MessageError("Unable to retrieve message header", e)

    # retrieves the body of a previously parsed XML message.
    #
    # return:
    #    Element representing the message body
    # raises:
    #    MessageError when any error occurs
    def getBody(self):
        try:
            return self.tree.find('body')
        except Exception,e:
            raise MessageError("Unable to retrieve message body", e)

    # retrieves the body of a previously parsed XML message.
    #
    # return:
    #    string containing the message body
    # raises:
    #    MessageError when any error occurs
    def getStrBody(self):
        try:
            return ET.tostring(self.tree.find('body'))
        except Exception,e:
            raise MessageError("Unable to retrieve message body", e)

    # retrieves the text embedded in the body of the message
    def getBodyText(self):
        raise MessageError("getBodyText(name) method has not been implemented")

    # retrieves the value of the named property from the message header.
    #
    # args:
    #    name: name attribute of property to retrieve
    # return:
    #    the value of the property if it exists
    #    otherwise None is returned
    # raise:
    #    MessageError when any error occurs
    def getProperty(self,name):
        raise MessageError("getProperty(name) method has not been implemented")

    # returns a list of dictionaries representing the properties from
    # the message header. The properties are returned in document order.
    # Each property is represented by a dictionary having the following
    # structure:
    #      {'name':<property name>,'value':<property value>}
    #
    # return:
    #    list containing the header properties
    # raise:
    #    MessageError when any error occurs
    def getProperties(self):
        path = 'header/properties'
        retval = []
        try:
            props = self.tree.findall(path)
            for prop in props:
                key = prop.get('name',None)
                val = prop.get('value',None)
                if key is not None and val is not None:
                    if self.bcode:
                        val = BA.a2b_hex(val)
                    retval.append({'name':key,'value':val})
        except Exception,e:
            raise MessageError("Unable to retrieve header properties", e)
        return retval

    # Returns the nested contents tags from the body of the message.
    # The contents are returned as a list of Element objects.
    #
    # returns
    #    list containing the body contents
    # raise:
    #    MessageError when any error occurs
    def getContents(self):
        path = 'body/contents'
        try:
            return self.tree.findall(path)
        except Exception,e:
            raise MessageError("Unable to retrieve body contents", e)

    # Returns a list of nested contents tags from the body of the message.
    # The individual contents tags are converted to XML strings.
    #
    # returns
    #    list containing the body contents
    # raise:
    #    MessageError when any error occurs
    def getStrContents(self):
        path = 'body/contents'
        try:
            return [ET.tostring(item) for item in self.tree.findall(path)]
        except Exception,e:
            raise MessageError("Unable to retrieve body contents", e)

    # Retrieves the text embedded in the message.
    def getMessageText(self):
        raise MessageError("getProperty(name) method has not been implemented")

    # Creates an message XML string with nested header and (optional) body elements.
    # The basic structure of the XML created is
    #     <message>
    #        <header />
    #        <body />
    #     </message>
    # 
    # Once created, use addProperty(name,value) or addProperties(dictionary) to
    # populate the header; use addBodyText(text) or addMessageText(text) to add a
    # <![CDATA[ ... ]]> block to the body or message.
    #
    # Note: After running this method, the tree and xml attributes are updated
    #       to the contain the newly created object.
    #
    # args:
    #    body: set true to include the body tag.
    # raise:
    #    MessageError when any error occurs     
    def initializeMessage(self,body=False):
        try:
            root = ET.Element('message')
            header = ET.SubElement(root, 'header')
            if body:
                body = ET.SubElement(root, 'body')
            self.tree = root
            self.xml = ET.tostring(root)
        except Exception,e:
            raise MessageError("Unable to generate message XML",e)

    # Adds a property tag with the specified name and value to the
    # header of the current message. This method will fail if the
    # tree attribute does not define a valid message. Normally, prior
    # to calling this method you should call create() to start the
    # message. This method can be called multiple times to add multiple
    # header properties to the message.
    #
    # This method can be called one of two ways:
    #     addProperty(attrib), or
    #     addProperty(name=<attribute name>,value=<attrubute value>)
    #
    # In the first option, attrib is a dictionary with the following structure:
    #       {'name':<property name>,'value':<property value>}
    #
    # args:
    #    attrib: dictionary defining the name & value for the property
    #    name:   name attribute for the property
    #    value:  value attribute for the property
    #    replace: flag. When False, the attribute will be added even if a
    #             property with the same name already exists. Otherwise,
    #             the attribute is not added.
    # raise:
    #    MessageError when any error occurs     
    def addProperty(self,attrib=None,name=None,value=None,replace=False):
        # verify valid arguments
        if (attrib is None or name is not None or value is not None) and \
           (attrib is not None or name is None or value is None):
            raise MessageError("Invalid arguments, must specify either attrib " + \
                               "or both name and value")
        # create attribute dictionary from arguments
        if attrib is None:
            attrib = {'name':str(name),'value':str(value)}
        elif not isinstance(attrib,types.DictType):
            raise MessageError("Invalid arguments, attrib must be a dictionary")
        # validate the dictionary
        if len(attrib) != 2 or 'name' not in attrib or 'value' not in attrib:
            raise MessageError("Invalid arguments, attrib must have attributes 'name' and 'value'")

        # add the property to the message
        if self.bcode:
            attrib['value'] = BA.b2a_hex(attrib.get('value'))
        self.__addProperty(attrib,replace)

    # Adds a list of properties to the message header. The messages are added
    # in the order they are defined in the list. Each property is defined via
    # a dictionary with the following structure:
    #       {'name':<property name>,'value':<property value>}
    #
    # Note: This method will fail if the tree attribute does not define a
    # valid message. Normally, prior to calling this method you should call
    # create() to start the message. This method can be called multiple times
    # to add multiple header properties to the message.
    #
    # args:
    #    list: list containing the property attributes
    #    replace: flag. When False, the attribute will be added even if a
    #             property with the same name already exists. Otherwise,
    #             the attribute is not added.
    # raise:
    #    MessageError when any error occurs     
    def addProperties(self,list,replace=False):
        # verify that the argument is an list 
        if not isinstance(list, types.ListType):
            raise MessageError("Invalid argument, must be a list of property dictionaries")
        # trim invalid contents from list - entries must be 1) a dictionary,
        # with 2) two entries and 3) have 'name' and 'value' entries
        list = [x for x in list if isinstance(x,types.DictType)]
        list = [x for x in list if len(x) == 2]
        list = [x for x in list if 'name' in x and 'value' in x]
        # make sure there are still some properties to add
        if len(list) == 0:
            raise MessageError("Invalid argument, list did not contain any valid entries")
        # now, add the properties to the tree
        for item in list:
            if self.bcode:
                item['value'] = BA.b2a_hex(item.get('value'))
            self.__addProperty(item,replace)
        pass

    # this is a private helper method that adds a property tag to the header
    # of the message. It is called by addProperty(...) and addProperties(...)
    # to create the property tag.
    #
    # args:
    #    attrib: the dictionary containing the attributes to add.
    #    replace: flag. When False, the attribute will be added even if a
    #             property with the same name already exists. Otherwise,
    #             the attribute is not added.
    # raise:
    #    MessageError when any error occurs     
    def __addProperty(self,attrib,replace=False):
        try:
            header = self.tree.find('header')
            if replace:
                name = attrib.get('name')
                if not self.__checkProperty(header,name):
                    ET.SubElement(header, 'properties', attrib)
            else:
                ET.SubElement(header, 'properties', attrib)
            self.xml = ET.tostring(self.tree)
        except Exception,e:
            raise MessageError("Unable to add property " + repr(attrib) + " to the message",e)
    
    # Checks to determine if the document tree contains a header property
    # with the specified name.
    #
    # args:
    #   tag: the tag enclosing the properties tags
    #   name: name attribute of the header property
    # return:
    #   True if the header has a property with the specified name, False otherwise
    # raise:
    #    MessageError when any error occurs     
    def __checkProperty(self,tag,name):
        try:
            props = tag.findall('properties')
            for prop in props:
                for key in prop.keys():
                    if key == 'name':
                        if prop.get(key) == name:
                            return True
            return False
        except Exception,e:
            raise MessageError("Encountered error checking property " + name,e)
        
    # Adds text to the body of the message. The text is added to the end of the body.
    # If the body already has text, the new text is appended to the existing text.
    #
    # Note: Where the text is appended depends on the actual body structure. If the
    # body is empty, the text is appended to the body; otherwise, the text is appended
    # as the "tail" of the last child of the body. 
    #
    # args:
    #    text: the text to add
    # raise:
    #    MessageError when any error occurs     
    def addBodyText(self,text):
        try:
            body = self.tree.find('body')
            kids = body.getchildren()
            if len(kids) == 0:
                if body.text == None:
                    body.text = text
                else:
                    body.text += text
            else:
                last = kids[-1]
                if last.tail == None:
                    last.tail = text
                else:
                    last.tail += text                
            self.xml = ET.tostring(self.tree)
        except Exception,e:
            raise MessageError("Unable to add text to message body",e)
 
    # Adds the specified text to the end of the message. If the message already
    # has text, the new text is appended to the existing text.
    #
    # Note: the text is actually appended as the "tail" of the last child
    # of the message. the effect is to always have the text at the end of the
    # message.
    #
    # args:
    #    text: the text to add
    # raise:
    #    MessageError when any error occurs
    def addMessageText(self,text):
        try:
            last = self.tree.getchildren()[-1]
            if last.tail == None:
                last.tail = text
            else:
                last.tail += text
            self.xml = ET.tostring(self.tree)
        except Exception,e:
            raise MessageError("Unable to add text to message",e)

##############################################################################
# General exception to be raised when errors occur in the message class.
##############################################################################
class MessageError(Exception):
    def __init__(self,value,cause=None):
          self.value = value
          self.cause = cause
    def __str__(self):
        msg = 'MessageError: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()