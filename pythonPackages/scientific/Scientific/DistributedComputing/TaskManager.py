#
# Task manager for distributed computing based on Pyro
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2008-4-16
#

import Pyro.core
import threading
import time

"""
Task manager for distributed computations. The task manager is used
by the module MasterSlave, but can also be used directly for different
distributed computing setups.
"""

debug = False

class TaskManagerTermination(Exception):

    """
    Signals that the task manager has no more tasks to handle.
    """

    pass

class TaskRaisedException(Exception):

    """
    Signals that an exception was raised inside a task. Four
    attributes provide information about the task and the exception:
    "task_id" is the task's id, "tag" is its tag, "exception"
    contains the original exception object, and "traceback"
    contains a text representation of the stack traceback at the
    time of the exception.
    """

    def __init__(self, task_id, tag, exception, traceback):
        self.task_id = task_id
        self.tag = tag
        self.exception = exception
        self.traceback = traceback

class Task(object):
    
    """
    Describes a task inside the task manager.
    """

    def __init__(self, tag, parameters, task_id):
        """
        @param tag: the tag of the computational task
        @type tag: C{str}
        @param parameters: the parameters of the task
        @type parameters: C{tuple}
        @param task_id: the task id
        @type task_id: C{str}
        """
        self.tag = tag
        self.parameters = parameters
        self.id = task_id
        self.requesting_processor = None
        self.handling_processor = None
        self.request_time = None
        self.start_time = None
        self.end_time = None

class TaskQueue(object):

    """
    A FIFO queue for tasks. This class is thread-safe.
    """

    def __init__(self):
        self.tasks = []
        self.tasks_by_tag = {}
        self.tasks_by_id = {}
        self.task_available = threading.Condition()
        self.terminate = False

    def __len__(self):
        """
        @returns: the number of tasks in the queue
        @rtype: C{int}
        """
        return len(self.tasks)

    def terminateWaitingThreads(self):
        """
        Makes all threads waiting for a task raise L{TaskManagerTermination}.
        """
        self.task_available.acquire()
        self.terminate = True
        self.task_available.notifyAll()
        self.task_available.release()

    def _checkForTermination(self):
        if self.terminate:
            self.task_available.release()
            raise TaskManagerTermination()

    def addTask(self, task, in_front=False):
        """
        @param task: the task to be added
        @type task: L{Task}
        @param in_front: if C{True}, add the task at the beginning of the
                         queue (this is for re-scheduling tasks that were
                         rejected or not properly handled). Otherwise, add
                         the task at the end of the queue.
        @type in_front: C{bool}
        """
        self.task_available.acquire()
        self.tasks.append(task)
        tasks = self.tasks_by_tag.setdefault(task.tag, [])
        if in_front:
            tasks.insert(0, task)
        else:
            tasks.append(task)
        self.tasks_by_id[task.id] = task
        self.task_available.notifyAll()
        self.task_available.release()

    def firstTask(self):
        """
        @returns: the first task in the queue
        @rtype: L{Task}

        Removes the first task from the queue and returns it. If the task queue
        is empty, the method blocks until a task becomes available.
        """
        self.task_available.acquire()
        while not self.tasks:
            self._checkForTermination()
            self.task_available.wait()
        task = self.tasks[0]
        self._removeTask(task)
        self.task_available.release()
        return task

    def firstTaskWithTag(self, tag):
        """
        @param tag: a task tag
        @type tag: C{str}
        @returns: the first task in the queue
        @rtype: L{Task}

        Removes the first task with the given tag from the queue and returns
        it. If no task with the requested tag is available, the method blocks
        until a matching task becomes available.
        """
        self.task_available.acquire()
        while not self.tasks_by_tag.get(tag, None):
            self._checkForTermination()
            self.task_available.wait()
        task = self.tasks_by_tag[tag][0]
        self._removeTask(task)
        self.task_available.release()
        return task

    def taskWithId(self, task_id):
        """
        @param task_id: a task id
        @type task_id: C{str}
        @returns: the task with the given task_id
        @rtype: L{Task}

        Removes the task with the given task_id from the queue and returns
        it. If the task is not in the queue, the method blocks
        until it becomes available.
        """
        self.task_available.acquire()
        while True:
            task = self.tasks_by_id.get(task_id, None)
            if task is not None:
                break
            self._checkForTermination()
            self.task_available.wait()
        self._removeTask(task)
        self.task_available.release()
        return task

    def _removeTask(self, task):
        self.tasks.remove(task)
        self.tasks_by_tag[task.tag].remove(task)
        del self.tasks_by_id[task.id]

    def taskCount(self):
        """
        @returns: a dictionary listing the number of tasks for each tag
        @rtype: C{dict}
        """
        self.task_available.acquire()
        count = {}
        for tag, tasks in self.tasks_by_tag.items():
            count[tag] = len(tasks)
        self.task_available.release()
        return count


class TaskManager(Pyro.core.ObjBase):

    """
    Manager for computational tasks.
    
    A TaskManager accepts task requests and hands them out to other processes.
    It stores the results that can then be picked up by the requester. A
    TaskManager also keeps track of its compute processes. If a process
    disappears, its running tasks are re-scheduled for execution by another
    compute process. TaskManangers are thread-safe.
    """

    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.id_counter = 0
        self.waiting_tasks = TaskQueue()
        self.running_tasks = TaskQueue()
        self.finished_tasks = TaskQueue()
        self.results = {}
        self.process_counter = 0
        self.active_processes = []
        self.process_info = []
        self.tasks_by_process = []
        self.data = {}
        self.lock = threading.Lock()
        self.watchdog = None

    def registerProcess(self, watchdog_period=None, info=None):
        """
        @param watchdog_period: the period at which the registering process
                                promises to ping the task manager to signal
                                that is still alive. If C{None}, no pings
                                are expected.
        @type watchdog_period: C{int} or C{NoneType}
        @param info: an information string telling something about the
                     machine running the process
        @type info: C{str}
        @returns: a unique process id
        @rtype: C{int}
        
        Registers a process with the task manager. All processes must call
        this method before making any other task manager calls.
        """
        self.lock.acquire()
        process_id = self.process_counter
        self.process_counter += 1
        self.active_processes.append(process_id)
        self.process_info.append(info)
        self.tasks_by_process.append([])
        self.lock.release()
        if watchdog_period is not None:
            if self.watchdog is None:
                self.watchdog = Watchdog(self)
            self.watchdog.registerProcess(process_id, watchdog_period)
        return process_id

    def unregisterProcess(self, process_id):
        """
        @param process_id: the id of the process
        @type process_id: C{int}
        
        Removes the process from the task manager's process list. All
        processes should unregister when they are no longer available
        for accepting tasks. The task manager will also unregister processes
        itself if they do not ping the task manager at the promised frequency.
        """
        if debug:
            print "Unregistering process", process_id
        self.lock.acquire()
        position = self.active_processes.index(process_id)
        del self.active_processes[position]
        del self.process_info[position]
        tasks = self.tasks_by_process[position]
        del self.tasks_by_process[position]
        self.lock.release()
        for t in tasks:
            self.returnTask(t.id)
        if self.watchdog is not None:
            self.watchdog.unregisterProcess(process_id)

    def ping(self, process_id):
        """
        @param process_id: the id of the process
        @type process_id: C{int}
        
        Tells the task manager that a process is still alive.
        """
        if self.watchdog is not None:
            self.watchdog.ping(process_id)

    def numberOfActiveProcesses(self):
        """
        @returns: the number of active processes
        @rtype: C{int}
        """
        return len(self.active_processes)

    def activeProcessInfo(self, pid):
        """
        @param pid: the number of an active process
        @type pid: C{int}
        @returns: information about the active process number pid
        @rtype: C{str}
        """
        return self.process_info[pid]

    def numberOfTasks(self):
        """
        @returns: a tuple of dictionaries containing the number of waiting
                  tasks, the number of running tasks, and the number of results
                  waiting to be retrieved. Each dictionary contains the
                  count for each tag.
        @rtype: C{tuple}
        """
        self.lock.acquire()
        waiting = self.waiting_tasks.taskCount()
        running = self.running_tasks.taskCount()
        finished = self.finished_tasks.taskCount()
        self.lock.release()
        return waiting, running, finished

    def addTaskRequest(self, tag, parameters, process_id=None):
        """
        @param tag: the tag of the task being requested
        @type tag: C{str}
        @param parameters: the parameters to be passed to the task
        @param process_id: the id of the requesting process (optional)
        @type process_id: C{int}
        @returns: the task id
        @rtype: C{str}
        """
        self.lock.acquire()
        task_id = tag + '_' + str(self.id_counter)
        self.id_counter += 1
        self.lock.release()
        new_task = Task(tag, parameters, task_id)
        if process_id:
            new_task.requesting_processor = process_id
        new_task.request_time = time.time()
        self.waiting_tasks.addTask(new_task)
        if debug:
            print "Task request %s: %s(%s)" % (task_id, tag, str(parameters))
        return task_id

    def getTaskWithTag(self, tag, process_id=None):
        """
        @param tag: a task tag
        @type tag: C{str}
        @param process_id: the id of the retrieving process (optional)
        @type process_id: C{int}
        @returns: the task id and the parameters
        @rtype: C{tuple}
        
        Returns a waiting task with the given tag. The task is removed from
        the list of waiting tasks and added to the list of running tasks.
        """
        task = self.waiting_tasks.firstTaskWithTag(tag)
        self._checkoutTask(task, process_id)
        return task.id, task.parameters

    def getAnyTask(self, process_id=None):
        """
        @param process_id: the id of the retrieving process (optional)
        @type process_id: C{int}
        @returns: the task id, the task tag, and the parameters
        @rtype: C{tuple}
        
        Returns a waiting task of arbitrary tag. The task is removed from
        the list of waiting tasks and added to the list of running tasks.
        """
        task = self.waiting_tasks.firstTask()
        self._checkoutTask(task, process_id)
        return task.id, task.tag, task.parameters

    def _checkoutTask(self, task, process_id):
        task.handling_processor = process_id
        task.start_time = time.time()
        self.running_tasks.addTask(task)
        if process_id is not None:
            self.lock.acquire()
            self.tasks_by_process[process_id].append(task)
            self.lock.release()
        if debug:
            print "Handing out task %s to process %s" \
                  % (task.id, str(process_id))

    def storeResult(self, task_id, result):
        """
        @param task_id: the id of the task for which the result is provided
        @type task_id: C{str}
        @param result: the result of the task
        
        Stores the result associated with the task. The task is removed from
        the list of running tasks and added to the list of finished tasks.
        """
        if debug:
            print "Task %s yielded result %s" % (task_id, result)
        self.lock.acquire()
        self.results[task_id] = result
        self.lock.release()
        task = self.running_tasks.taskWithId(task_id)
        task.end_time = time.time()
        task.completed = True
        self.finished_tasks.addTask(task)
        self._removeTask(task)

    def storeException(self, task_id, exception, traceback):
        """
        @param task_id: the id of the task for which the result is provided
        @type task_id: C{str}
        @param exception: the exception raised by the task
        @param traceback: a text version of the stack traceback at the time
                          of the exception
        @type traceback: C{str}

        Stores the exception associated with the task. The task is removed from
        the list of running tasks and added to the list of finished tasks.
        When the result is retrieved by another process, L{TaskRaisedException}
        is raised.
        """
        if debug:
            print "Task %s raised exception %s" % (task_id, exception)
        self.lock.acquire()
        self.results[task_id] = (exception, traceback)
        self.lock.release()
        task = self.running_tasks.taskWithId(task_id)
        task.end_time = time.time()
        task.completed = False
        self.finished_tasks.addTask(task)
        self._removeTask(task)

    def returnTask(self, task_id):
        """
        @param task_id: the id of the task for which the result is provided
        @type task_id: C{str}

        Removes a task from the list of running tasks and put its back at the
        beginning of the list of waiting tasks. This method should be called
        by a process that has obtained a task but cannot handle it.
        """
        if debug:
            print "Task %s returned" % task_id
        task = self.running_tasks.taskWithId(task_id)
        self._removeTask(task)
        task.start_time = None
        task.handling_processor = None
        self.waiting_tasks.addTask(task, in_front=True)
        
    def _removeTask(self, task):
        if task.handling_processor is not None:
            self.lock.acquire()
            try:
                self.tasks_by_process[task.handling_processor].remove(task)
            except ValueError:
                pass
            self.lock.release()

    def getAnyResult(self):
        """
        @returns: the task id, the task tag, and the result of the task
        @rtype: C{tuple}
        
        Returns the result of an arbitrary finished task. The task is removed
        from the list of finished tasks.
        """
        task = self.finished_tasks.firstTask()
        result = self.results[task.id]
        del self.results[task.id]
        if task.completed:
            return task.id, task.tag, result
        else:
            raise TaskRaisedException(task.id, task.tag, result[0], result[1])

    def getResultWithTag(self, tag):
        """
        @param tag: a task tag
        @returns: the task id and the result of the task
        @rtype: C{tuple}
        
        Returns the result of a finished task that has the given tag. The
        task is removed from the list of finished tasks.
        """
        task = self.finished_tasks.firstTaskWithTag(tag)
        result = self.results[task.id]
        del self.results[task.id]
        if debug:
            print "Handed out result of %s" % task.id
        if task.completed:
            return task.id, result
        else:
            raise TaskRaisedException(task.id, task.tag, result[0], result[1])

    def storeData(self, **kw):
        """
        @param kw: a keyword list of data items to be stored
        @type kw: C{dict}
        
        This routine permits processes to exchange arbitrary data items
        through the task manager.
        """
        self.lock.acquire()
        for label, data in kw.items():
            self.data[label] = data
        self.lock.release()

    def retrieveData(self, label):
        """
        @param label: the label of the data item to be retrieved
        @type label: C{str}
        """
        self.lock.acquire()
        data = self.data[label]
        self.lock.release()
        return data

    def deleteData(self, label):
        """
        @param label: the label of the data item to be deleted
        @type label: C{str}
        """
        self.lock.acquire()
        del self.data[label]
        self.lock.release()

    def terminate(self):
        """
        Signals that no more tasks or results will be requested. All waiting
        threads will be terminated by raising L{TaskManagerTermination}.
        """
        if debug:
            print "Terminating"
        self.waiting_tasks.terminateWaitingThreads()
        self.running_tasks.terminateWaitingThreads()
        self.finished_tasks.terminateWaitingThreads()


class Watchdog(object):

    """
    A background process that watches compute tasks and unregisters those
    that do not ping the task manager at the promised interval.
    """

    def __init__(self, task_manager):
        self.task_manager = task_manager
        self.ping_period = {}
        self.last_ping = {}
        self.done = False
        self.lock = threading.Lock()
        self.background_thread = threading.Thread(target = self.watchdogThread)
        self.background_thread.setDaemon(True)
        self.thread_started = False

    def registerProcess(self, process_id, ping_period):
        self.lock.acquire()
        self.ping_period[process_id] = ping_period
        self.last_ping[process_id] = time.time()
        if not self.thread_started:
            self.background_thread.start()
            self.thread_started = True
        self.lock.release()

    def unregisterProcess(self, process_id):
        self.lock.acquire()
        try:
            del self.ping_period[process_id]
            del self.last_ping[process_id]
        except KeyError:
            # KeyError happens when processes without watchdog are unregistered
            pass
        self.lock.release()

    def ping(self, process_id):
        self.lock.acquire()
        self.last_ping[process_id] = time.time()
        self.lock.release()

    def terminate(self, blocking=False):
        self.done = True
        if blocking:
            self.background_thread.join()

    def watchdogThread(self):
        while True:
            now = time.time()
            dead_processes = []
            min_delay = min(self.ping_period.values() + [60.])
            self.lock.acquire()
            for process_id in self.ping_period.keys():
                delay = now-self.last_ping[process_id]
                if delay > 4*self.ping_period[process_id]:
                    dead_processes.append(process_id)
            self.lock.release()
            for process_id in dead_processes:
                if debug:
                    print "Process %d died" % process_id
                self.task_manager.unregisterProcess(process_id)
            if self.done:
                break
            time.sleep(min_delay)

