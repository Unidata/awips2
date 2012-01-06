/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
*/

#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <string.h>
#include <sasl/sasl.h>
#include <ruby.h>

static VALUE mSasl;

#define INPUT_SIZE 512
#define MECH_SIZE 32

typedef void* sasl_context_t;

#define QSASL_OK 0
#define QSASL_CONTINUE 1
#define QSASL_FAILED 2 

typedef struct {
    char magic[8];
    sasl_conn_t*  conn;
    sasl_callback_t callbacks[8];
    char* userName;
    char* password;
    char* operUserName;
    unsigned int minSsf;
    unsigned int maxSsf;
    char mechanism[MECH_SIZE];
    char input[INPUT_SIZE];
} context_t;

//
// Resolve forward references
//
static VALUE qsasl_free(int, VALUE*, VALUE);

//
// Validate an input string to ensure that it is either NULL or of reasonable size.
//
static int qsasl_valid(char* str)
{
    int idx;

    if (str == 0)
        return 1;

    for (idx = 0; idx < INPUT_SIZE; idx++) {
        if (str[idx] == '\0')
            return 1;
    }

    return 0;
}

//
// SASL callback for identity and authentication identity.
//
static int qsasl_cb_user(void* _context, int id, const char **result, unsigned *len)
{
    context_t* context = (context_t*) _context;

    if (context->userName)
        *result = context->userName;

    return SASL_OK;
}

//
// SASL callback for passwords.
//
static int qsasl_cb_password(sasl_conn_t* conn, void* _context, int id, sasl_secret_t **psecret)
{
    context_t* context = (context_t*) _context;
    sasl_secret_t* secret;
    size_t length;

    if (context->password)
        length = strlen(context->password);
    else
        length = 0;

    secret = (sasl_secret_t*) malloc(sizeof(sasl_secret_t) + length);
    secret->len = length;
    if (length)
        memcpy(secret->data, context->password, length);
    *psecret = secret;

    return SASL_OK;
}

//
// Interactively prompt the user for authentication data.
//
static void qsasl_prompt(sasl_context_t _context, sasl_interact_t* interact)
{
    context_t* context = (context_t*) _context;
    char *pass;
    char *input;
    char passwdPrompt[100];

    if (interact->id == SASL_CB_PASS) {
        strncpy(passwdPrompt, interact->prompt, 95);
        strcat(passwdPrompt, ": ");
        pass = getpass(passwdPrompt);
        strncpy(context->input, pass, INPUT_SIZE - 1);
        context->input[INPUT_SIZE - 1] = '\0';
    } else {
        printf(interact->prompt);
        if (interact->defresult) {
            printf(" (%s)", interact->defresult);
        }
        printf(": ");
        input = fgets(context->input, INPUT_SIZE, stdin);
        if (input != context->input) {
            rb_raise(rb_eRuntimeError, "Unexpected EOF on interactive prompt");
        }
    }

    interact->result = context->input;
    interact->len = strlen(context->input);
}

//
// Initialize the SASL client library.
//
static VALUE qsasl_client_init()
{
    int result;

    result = sasl_client_init(0);
    if (result != SASL_OK)
        rb_raise(rb_eRuntimeError,
                 "sasl_client_init failed: %d - %s",
                 result, sasl_errstring(result, -0, 0));
    return Qnil;
}

//
// Allocate a new SASL client context.
//
static VALUE qsasl_client_new(int argc, VALUE *argv, VALUE obj)
{
    char* mechanism = 0;
    char* serviceName = 0;
    char* hostName = 0;
    char* userName = 0;
    char* password = 0;
    unsigned int minSsf = 0;
    unsigned int maxSsf = 65535;

    int result;
    int i = 0;
    context_t *context;
    sasl_security_properties_t secprops;

    if (argc != 7)
        rb_raise(rb_eRuntimeError, "Wrong number of arguments");

    if (!NIL_P(argv[0]))
        mechanism = StringValuePtr(argv[0]);
    if (!NIL_P(argv[1]))
        serviceName = StringValuePtr(argv[1]);
    if (!NIL_P(argv[2]))
        hostName = StringValuePtr(argv[2]);
    if (!NIL_P(argv[3]))
        userName = StringValuePtr(argv[3]);
    if (!NIL_P(argv[4]))
        password = StringValuePtr(argv[4]);
    minSsf = FIX2INT(argv[5]);
    maxSsf = FIX2INT(argv[6]);

    if (!qsasl_valid(mechanism) || !qsasl_valid(serviceName) ||
        !qsasl_valid(hostName) || !qsasl_valid(userName) ||
        !qsasl_valid(password)) {
        rb_raise(rb_eRuntimeError, "Invalid string argument");
    }

    context = (context_t*) malloc(sizeof(context_t));
    memset(context, 0, sizeof(context_t));
    strcpy(context->magic, "QSASL01");

    context->minSsf = minSsf;
    context->maxSsf = maxSsf;
    if (mechanism != 0) {
        strncpy(context->mechanism, mechanism, MECH_SIZE - 1);
        context->mechanism[MECH_SIZE - 1] = '\0';
    }

    context->callbacks[i].id = SASL_CB_GETREALM;
    context->callbacks[i].proc = 0;
    context->callbacks[i++].context = 0;

    if (userName != 0 && userName[0] != '\0') {
        context->userName = (char*) malloc(strlen(userName) + 1);
        strcpy(context->userName, userName);

        context->callbacks[i].id = SASL_CB_USER;
        context->callbacks[i].proc = qsasl_cb_user;
        context->callbacks[i++].context = context;
        
        context->callbacks[i].id = SASL_CB_AUTHNAME;
        context->callbacks[i].proc = qsasl_cb_user;
        context->callbacks[i++].context = context;
    }

    context->callbacks[i].id = SASL_CB_PASS;
    if (password != 0 && password[0] != '\0') {
        context->password = (char*) malloc(strlen(password) + 1);
        strcpy(context->password, password);

        context->callbacks[i].proc = qsasl_cb_password;
    } else
        context->callbacks[i].proc = 0;
    context->callbacks[i++].context = context;

    context->callbacks[i].id = SASL_CB_LIST_END;
    context->callbacks[i].proc = 0;
    context->callbacks[i++].context = 0;
    
    result = sasl_client_new(serviceName, hostName, 0, 0,
                             context->callbacks, 0, &context->conn);

    if (result != SASL_OK) {
        context->conn = 0;
        qsasl_free(1, (VALUE*) &context, Qnil);
        rb_raise(rb_eRuntimeError, "sasl_client_new failed: %d - %s",
                 result, sasl_errstring(result, 0, 0));
    }

    secprops.min_ssf = minSsf;
    secprops.max_ssf = maxSsf;
    secprops.maxbufsize = 65535;
    secprops.property_names = 0;
    secprops.property_values = 0;
    secprops.security_flags = 0;//TODO: provide means for application to configure these

    result = sasl_setprop(context->conn, SASL_SEC_PROPS, &secprops);
    if (result != SASL_OK) {
        qsasl_free(1, (VALUE*) &context, Qnil);
        rb_raise(rb_eRuntimeError, "sasl_setprop failed: %d - %s",
                 result, sasl_errdetail(context->conn));
    }

    return (VALUE) context;
}

//
// Free a SASL client context.
//
static VALUE qsasl_free(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;

    if (argc == 1)
        context = (context_t*) argv[0];
    else
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");

    if (context->conn)
        sasl_dispose(&context->conn);
    if (context->userName)
        free(context->userName);
    if (context->password)
        free(context->password);
    if (context->operUserName)
        free(context->operUserName);
    free(context);

    return Qnil;
}

//
// Start the SASL exchange from the client's point of view.
//
static VALUE qsasl_client_start(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;
    char* mechList;
    char* mechToUse;
    int result;
    int propResult;
    const char* response;
    unsigned int len;
    sasl_interact_t* interact = 0;
    const char* chosen;
    const char* operName;

    if (argc == 2) {
        context = (context_t*) argv[0];
        mechList = StringValuePtr(argv[1]);
    } else
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");

    if (strlen(context->mechanism) == 0)
        mechToUse = mechList;
    else
        mechToUse = context->mechanism;

    do {
        result = sasl_client_start(context->conn, mechToUse, &interact,
                                   &response, &len, &chosen);
        if (result == SASL_INTERACT) {
            qsasl_prompt(context, interact);
        }
    } while (result == SASL_INTERACT);

    if (result != SASL_OK && result != SASL_CONTINUE)
        rb_raise(rb_eRuntimeError, "sasl_client_start failed: %d - %s",
                 result, sasl_errdetail(context->conn));

    if (result == SASL_OK) {
        propResult = sasl_getprop(context->conn, SASL_USERNAME, (const void**) &operName);
        if (propResult == SASL_OK) {
            context->operUserName = (char*) malloc(strlen(operName) + 1);
            strcpy(context->operUserName, operName);
        }
    }

    return rb_ary_new3(3, INT2NUM(result), rb_str_new(response, len), rb_str_new2(chosen));
}

//
// Take a step in the SASL exchange (only needed for multi-challenge mechanisms).
//
static VALUE qsasl_client_step(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;
    VALUE challenge;
    int result;
    int propResult;
    const char* response;
    const char* operName;
    unsigned int len;
    sasl_interact_t* interact = 0;

    if (argc == 2) {
        context = (context_t*) argv[0];
        challenge = argv[1];
    }
    else
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");

    do {
        result = sasl_client_step(context->conn,
                                  RSTRING(challenge)->ptr, RSTRING(challenge)->len,
                                  &interact, &response, &len);
        if (result == SASL_INTERACT) {
            qsasl_prompt(context, interact);
        }
    } while (result == SASL_INTERACT);

    if (result != SASL_OK && result != SASL_CONTINUE)
        return QSASL_FAILED;

    if (result == SASL_OK) {
        propResult = sasl_getprop(context->conn, SASL_USERNAME, (const void**) &operName);
        if (propResult == SASL_OK) {
            context->operUserName = (char*) malloc(strlen(operName) + 1);
            strcpy(context->operUserName, operName);
        }
    }

    return rb_ary_new3(2, INT2NUM(result), rb_str_new(response, len));
}

static VALUE qsasl_user_id(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;

    if (argc == 1) {
        context = (context_t*) argv[0];
    } else {
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");
    }

    if (context->operUserName)
        return rb_str_new2(context->operUserName);

    return Qnil;
}

//
// Encode transport data for the security layer.
//
static VALUE qsasl_encode(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;
    VALUE clearText;
    const char* outBuffer;
    unsigned int outSize;
    int result;

    if (argc == 2) {
        context = (context_t*) argv[0];
        clearText = argv[1];
    }
    else
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");

    result = sasl_encode(context->conn,
                         RSTRING(clearText)->ptr, RSTRING(clearText)->len,
                         &outBuffer, &outSize);
    if (result != SASL_OK)
        rb_raise(rb_eRuntimeError, "sasl_encode failed: %d - %s",
                 result, sasl_errdetail(context->conn));

    return rb_str_new(outBuffer, outSize);
}

//
// Decode transport data for the security layer.
//
static VALUE qsasl_decode(int argc, VALUE *argv, VALUE obj)
{
    context_t* context;
    VALUE cipherText;
    const char* outBuffer;
    unsigned int outSize;
    int result;

    if (argc == 2) {
        context = (context_t*) argv[0];
        cipherText = argv[1];
    }
    else
        rb_raise(rb_eRuntimeError, "Wrong Number of Arguments");

    result = sasl_decode(context->conn,
                         RSTRING(cipherText)->ptr, RSTRING(cipherText)->len,
                         &outBuffer, &outSize);
    if (result != SASL_OK)
        rb_raise(rb_eRuntimeError, "sasl_decode failed: %d - %s",
                 result, sasl_errdetail(context->conn));

    return rb_str_new(outBuffer, outSize);
}

//
// Initialize the Sasl module.
//
void Init_sasl()
{
    mSasl = rb_define_module("Sasl");

    rb_define_module_function(mSasl, "client_init", qsasl_client_init, -1);
    rb_define_module_function(mSasl, "client_new", qsasl_client_new, -1);
    rb_define_module_function(mSasl, "free", qsasl_free, -1);
    rb_define_module_function(mSasl, "client_start", qsasl_client_start, -1);
    rb_define_module_function(mSasl, "client_step", qsasl_client_step, -1);
    rb_define_module_function(mSasl, "user_id", qsasl_user_id, -1);
    rb_define_module_function(mSasl, "encode", qsasl_encode, -1);
    rb_define_module_function(mSasl, "decode", qsasl_decode, -1);
}
