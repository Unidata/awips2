#!/bin/bash
g++ edexBridge.cpp -I/awips2/ldm/src/pqact \
   -I/awips2/ldm/include \
   -I/awips2/ldm/src \
   -I/awips2/qpid/include \
   -L/awips2/ldm/lib \
   -L/awips2/qpid/lib \
   -l ldm -l xml2 -l qpidclient -l qpidmessaging -l qpidcommon -l qpidtypes -o edexBridge
cp edexBridge ../patch/bin/
