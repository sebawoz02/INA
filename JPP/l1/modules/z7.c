#include "z7.h"
#include <Python.h>

PyObject *pModule;

void z7_module_init(void)
{
    Py_Initialize();
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    PyObject *pName = PyUnicode_FromString("modules.z3");
    pModule = PyImport_Import(pName);
    if(pModule == NULL) 
    {
        PyErr_Print();
        fprintf(stderr, "Could not load module z3\n");
    }
}

uint64_t factorial_iterative(const uint8_t n)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = PyObject_GetAttrString(pModule, "factorial_iterative");
    uint64_t result;

    if (pFunc && PyCallable_Check(pFunc)) {
        pArgs = PyTuple_New(1);
        PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n));

        pValue = PyObject_CallObject(pFunc, pArgs);
        Py_DECREF(pArgs);

        if (pValue != NULL) {
            result = PyLong_AsLong(pValue);
            Py_DECREF(pValue);
        } else {
            PyErr_Print();
        }
    } else {
        if (PyErr_Occurred()) PyErr_Print();
        fprintf(stderr, "Could not find function 'factorial_iterative'\n");
    }
    Py_XDECREF(pFunc);
    return result;
}

uint64_t factorial_recursive(const uint8_t n)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = PyObject_GetAttrString(pModule, "factorial_recursive");
    uint64_t result;

    if (pFunc && PyCallable_Check(pFunc)) {
        pArgs = PyTuple_New(1);
        PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n));

        pValue = PyObject_CallObject(pFunc, pArgs);
        Py_DECREF(pArgs);

        if (pValue != NULL) {
            result = PyLong_AsLong(pValue);
            Py_DECREF(pValue);
        } else {
            PyErr_Print();
        }
    } else {
        if (PyErr_Occurred()) PyErr_Print();
        fprintf(stderr, "Could not find function 'factorial_recursive'\n");
    }
    Py_XDECREF(pFunc);
    return result;
}

uint64_t gcd_iterative(uint64_t a, uint64_t b)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = PyObject_GetAttrString(pModule, "gcd_iterative");
    uint64_t result;

    if (pFunc && PyCallable_Check(pFunc)) {
        pArgs = PyTuple_New(2);
        PyTuple_SetItem(pArgs, 0, PyLong_FromLong(a));
        PyTuple_SetItem(pArgs, 1, PyLong_FromLong(b));

        pValue = PyObject_CallObject(pFunc, pArgs);
        Py_DECREF(pArgs);

        if (pValue != NULL) {
            result = PyLong_AsLong(pValue);
            Py_DECREF(pValue);
        } else {
            PyErr_Print();
        }
    } else {
        if (PyErr_Occurred()) PyErr_Print();
        fprintf(stderr, "Could not find function 'gcd_iterative'\n");
    }
    Py_XDECREF(pFunc);
    return result;
}

uint64_t gcd_recursive(const uint64_t a, const uint64_t b)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = PyObject_GetAttrString(pModule, "gcd_recursive");
    uint64_t result;

    if (pFunc && PyCallable_Check(pFunc)) {
        pArgs = PyTuple_New(2);
        PyTuple_SetItem(pArgs, 0, PyLong_FromLong(a));
        PyTuple_SetItem(pArgs, 1, PyLong_FromLong(b));

        pValue = PyObject_CallObject(pFunc, pArgs);
        Py_DECREF(pArgs);

        if (pValue != NULL) {
            result = PyLong_AsLong(pValue);
            Py_DECREF(pValue);
        } else {
            PyErr_Print();
        }
    } else {
        if (PyErr_Occurred()) PyErr_Print();
        fprintf(stderr, "Could not find function 'gcd_recursive'\n");
    }
    Py_XDECREF(pFunc);
    return result;
}

void z7_module_destroy(void)
{
    Py_DECREF(pModule);    
    Py_Finalize();
}
