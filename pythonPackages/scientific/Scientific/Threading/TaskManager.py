# This module provides a simple task manager for running parallel
# calculations on shared-memory machines.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-6-12
#

"""
Parallel task manager for shared-memory multiprocessor machines

@undocumented: Task
"""

import threading

class TaskManager:

    """
    Parallel task manager for shared-memory multiprocessor machines

    This class provides a rather simple way to profit from
    shared-memory multiprocessor machines by running several tasks
    in parallel. The calling program decides how many execution threads
    should run at any given time, and then feeds compute tasks to
    the task manager, who runs them as soon as possible without exceeding
    the maximum number of threads.

    The major limitation of this approach lies in Python's Global
    Interpreter Lock. This effectively means that no more than one
    Python thread can run at the same time. Consequently, parallelization
    can only be achieved if the tasks to be parallelized spend
    significant time in C extension modules that release the Global
    Interpreter Lock.
    """

    def __init__(self, nthreads):
        """
        @param nthreads: the maximum number of compute threads that should
                         run in parallel. Note: This does not include the
                         main thread which generated and feeds the task
                         manager!
        @type nthreads: C{int}
        """
        self.nthreads = nthreads
        self.waiting_tasks = []
        self.running_tasks = []
        self.lock = threading.Lock()
        self.data_lock = threading.Lock()
        self.can_run = threading.Condition(self.lock)
        self.can_submit = threading.Condition(self.lock)
        self.task_available = threading.Condition(self.lock)
        self.scheduler = threading.Thread(target=self._scheduler)
        self.scheduler.start()

    def runTask(self, function, args):
        """
        Run a task as soon as processing capacity becomes available

        @param function: the function that will be executed as the body of
                         the task
        @type function: callable
        @param args: the arguments that will be passed to function when it
                     is called. An additional argument will be added at the
                     end: a lock object that the task can use to get
                     temporarily exclusive access to data shared with other
                     tasks.
        @type args: C{tuple}
        """
        self.can_submit.acquire()
        if len(self.waiting_tasks) >= self.nthreads:
            self.can_submit.wait()
        self.can_submit.release()

        task = Task(self, function, args + (self.data_lock,))

        self.task_available.acquire()
        self.waiting_tasks.append(task)
        self.task_available.notify()
        self.task_available.release()

    def terminate(self):
        """
        Wait until all tasks have finished
        """
        self.task_available.acquire()
        self.waiting_tasks.append(None)
        self.task_available.notify()
        self.task_available.release()
        self.scheduler.join()
        done = 0
        while not done:
            self.can_run.acquire()
            if self.running_tasks:
                self.can_run.wait()
            done = len(self.running_tasks) == 0
            self.can_run.release()

    def _removeTask(self, task):
        self.can_run.acquire()
        self.running_tasks.remove(task)
        self.can_run.notifyAll()
        self.can_run.release()
        
    def _scheduler(self):
        while 1:
            self.task_available.acquire()
            if not self.waiting_tasks:
                self.task_available.wait()
            self.task_available.release()

            self.can_run.acquire()
            while len(self.running_tasks) >= self.nthreads:
                self.can_run.wait()
            task = self.waiting_tasks[0]
            del self.waiting_tasks[0]
            if task is not None:
                self.running_tasks.append(task)
                task.start()
            self.can_submit.notify()
            self.can_run.release()
            if task is None:
                break

class Task(threading.Thread):

    def __init__(self, manager, function, args):
        self.__task_manager = manager
        self.__function = function
        self.__args = args
        threading.Thread.__init__(self)

    def run(self):
        apply(self.__function, self.__args)
        self.__task_manager._removeTask(self)


# Test code

if __name__ == '__main__':

    import time
    from random import randint

    def dummy(n, results, lock):
        print n, "running"
        time.sleep(randint(1, 5))
        lock.acquire()
        results.append(n)
        lock.release()
        print n, "finished"

    m = TaskManager(2)
    results = []
    for i in range(5):
        m.runTask(dummy, (i, results))
    m.terminate()
    print "All finished"
    print results
