/****************************************************************************
** Meta object code from reading C++ file 'TQTcpServer.h'
**
** Created: Thu Oct 11 18:00:54 2012
**      by: The Qt Meta Object Compiler version 63 (Qt 4.8.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "TQTcpServer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'TQTcpServer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_apache__thrift__async__TQTcpServer[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      36,   35,   35,   35, 0x08,
      54,   35,   35,   35, 0x08,
      68,   35,   35,   35, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_apache__thrift__async__TQTcpServer[] = {
    "apache::thrift::async::TQTcpServer\0\0"
    "processIncoming()\0beginDecode()\0"
    "socketClosed()\0"
};

void apache::thrift::async::TQTcpServer::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        TQTcpServer *_t = static_cast<TQTcpServer *>(_o);
        switch (_id) {
        case 0: _t->processIncoming(); break;
        case 1: _t->beginDecode(); break;
        case 2: _t->socketClosed(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData apache::thrift::async::TQTcpServer::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject apache::thrift::async::TQTcpServer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_apache__thrift__async__TQTcpServer,
      qt_meta_data_apache__thrift__async__TQTcpServer, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &apache::thrift::async::TQTcpServer::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *apache::thrift::async::TQTcpServer::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *apache::thrift::async::TQTcpServer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_apache__thrift__async__TQTcpServer))
        return static_cast<void*>(const_cast< TQTcpServer*>(this));
    return QObject::qt_metacast(_clname);
}

int apache::thrift::async::TQTcpServer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 3)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
