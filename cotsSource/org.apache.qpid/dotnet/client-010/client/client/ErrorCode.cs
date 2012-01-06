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

using System;

namespace org.apache.qpid.client
{
    public enum QpidErrorCode
    {
        NO_ERROR = 200,
        CONTENT_TOO_LARGE = 311,
        NO_ROUTE = 312,
        NO_CONSUMERS = 313,
        CONNECTION_FORCED = 320,
        INVALID_PATH = 402,
        ACCESS_REFUSED = 403,
        NOT_FOUND = 404,
        RESOURCE_LOCKED = 405,
        PRE_CONDITION_FAILED = 406,
        FRAME_ERROR = 501,
        SYNTAX_ERROR = 502,
        COMMAND_INVALID = 503,
        SESSION_ERROR = 504,
        NOT_ALLOWED = 530,
        NOT_IMPLEMENTED = 540,
        INTERNAL_ERROR = 541,
        INVALID_ARGUMENT = 542,
        UNDEFINED = 1
    }

    public struct ErrorCode
    {
        private int _code;
        private String _desc;
        private readonly bool _hardError;

        public ErrorCode(int code, String desc, bool hardError)
        {
            _code = code;
            _desc = desc;
            _hardError = hardError;
        }

        public int Code
        {
            get { return _code; }
            set { _code = value; }
        }

        public String Description
        {
            get { return _desc; }
            set { _desc = value; }
        }

        public bool ISHardError
        {
            get { return _hardError; }
        }

        public static ErrorCode GetErrorCode(int code)
        {
            switch (code)
            {
                case 200:
                    return
                        new ErrorCode(200, "reply-success", true);
                case 311:
                    return
                        new ErrorCode(311, "content-too-large", false);
                case 312:
                    return
                        new ErrorCode(312, "no-route", false);
                case 313:
                    return
                        new ErrorCode(313, "content-consumers", false);
                case 320:
                    return
                        new ErrorCode(320, "connection-forced", true);
                case 402:
                    return
                        new ErrorCode(402, "invalid-path", true);
                case 403:
                    return
                        new ErrorCode(403, "access-refused", false);
                case 404:
                    return
                        new ErrorCode(404, "not-found", false);
                case 405:
                    return
                        new ErrorCode(405, "resource-locked", false);
                case 406:
                    return
                        new ErrorCode(406, "precondition-failed", false);
                case 501:
                    return
                        new ErrorCode(501, "frame_error", true);
                case 502:
                    return
                        new ErrorCode(502, "syntax_error", true);
                case 503:
                    return
                        new ErrorCode(503, "command_invalid", true);
                case 504:
                    return
                        new ErrorCode(504, "sesion_error", true);
                case 530:
                    return
                        new ErrorCode(530, "not_allowed", true);
                case 540:
                    return
                        new ErrorCode(540, "not_implemented", true);
                case 541:
                    return
                        new ErrorCode(541, "internal_error", true);
                case 542:
                    return
                        new ErrorCode(542, "invalid_argument", true);
                default:
                    return new ErrorCode(1, "undefined", true);
            }
        }
    }
}
