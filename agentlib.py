from thespian.actors import *
from thespian.transient import transient_idle

from time import sleep
import json

from pdb import set_trace

import pycouchdb
import requests
import pydantic
import subprocess
import multiprocessing as mp
import time
from datetime import timedelta
from uuid import uuid4
import sys

import logging

logger = logging.getLogger(__name__)
handler = logging.StreamHandler(sys.stdout)
logger.addHandler(handler)
logger.setLevel(logging.DEBUG)


baseurl = 'http://host.containers.internal:8081/v1/chat/completions'
#http://localhost:8081/v1/chat/completions

use_couchdb = False
if use_couchdb:
    username = 'assistant_user'
    psw = '1agR99i4XtGXfsK5RvMl'
    #server = pycouchdb.Server(f"http://{username}:{psw}@localhost:5984/")
    del username
    del psw

    cdb = server.database("messages2")

class DB:
    def __setitem__(self, k, v):
        if use_couchdb:
            cdb.save({"_id": v, "value": v})
        else:
            pass
    def __getitem__(self, k):
        return ''#cdb.get(k) 

db = DB()

def nid():
    return str(uuid4())

def snid():
    ''' session id '''
    return 'a' #str(uuid4())[:4]

def multiline_input(prompt, max_lines):
    print(prompt, end='')

    lines = []
    for i in range(max_lines):
        try:
            line = input()
            lines.append(line)
        except EOFError:
            break

    text = '\n'.join(lines)
    return text + '\n'


def faketool(*args, **kwargs):
    ''' description '''

    print(*args)
    print(**kwargs)
    sleep(3)
    return 1

sysprompt = "You are a helpful assistant"

class Traj:
    ''' class to manage tracking the full trajectory '''
    def __init__(self, prompt):
        sp = {"role": "system", "content":sysprompt}
        self.msgs = [sp]
        self.state = 'system'
        self.add_prompt(prompt)
        ''' state machine '''

    def get_prompt(self):
        ''' get prompt, formatted correctly for llamacpp '''
        prompt_dict = {'messages': self.msgs,
                  "tools": get_tools()
                }
        return prompt_dict
        '''
        prompt = {"prompt": str(message), 
                  "n_predict":6000, 
                  "temperature": 0.6,
                  "slot_id":1,
                  "system_promot": sysprompt,
                  "seed":42,
                  "tools": tools,
                  "top_k": 40,
                  "top_p": 0.95,
                  "min_p": 0.0,
                  }
        '''

    def add_completion(self, m, wrap=True):
        if self.state != 'prompt':
            raise RuntimeError('cant add completion after {self.state}')
        self.state = 'completed'
        if wrap:
            m = {'role':'assistant', 'content':m}
        self.msgs += [m]
        return self


    def add_toolresult(self, m, wrap=True):
        if self.state != 'prompt':
            raise RuntimeError('cant add toolresult after {self.state}')
        self.state = 'toolresult'
        if wrap:
            m = {'role':'tool', 'content':m}
        self.msgs += [m]

    def add_prompt(self, m, wrap=True):
        if self.state not in ['completed','toolresult','system']:
            raise RuntimeError('cant add prompt after {self.state}')
        self.state = 'prompt'
        if wrap:
            m = {'role':'user', 'content':m}
        self.msgs += [m]
        return self


#@transient_idle(timedelta(seconds=3))
class Tool(Actor):
    def __init__(self):
        self.name = "python interpreter"

    def receiveMessage(self, message, sender):
        if isinstance(message, dict):
            args = message['call']
            tr = message['tr']
            toolresult = {'type':'toolresult', 'content':"toolresult: python ran sucessfully and returned the number 2033", 'tr':tr}
            self.send(sender, toolresult)


def completion(prompt):
    logger.info(prompt)
    ret = requests.post(baseurl, data=json.dumps(prompt))
    ret.raise_for_status()
    #logger.debug(f'completion returned: {ret.json()}')
    return ret.json()


class Toolimpl:
    def __init__(self):
        pass
    def template(self):
            return {
            "type":"function",
            "function":{
                "name":"python",
                "description":"Runs code in an ipython interpreter and returns the result of the execution after 60 seconds.",
                "parameters":{
                    "type":"object",
                    "properties":{
                        "code":{
                        "type":"string",
                        "description":"The code to run in the ipython interpreter."
                        }
                    },
                "required":["code"] # required args
                }
                }
            }

def get_tools():
    tools = [ Toolimpl().template() ]
    return tools

def python_tool(*, code=None):
    return 'bla'

def read_file_section(*, filename=None, ):
    with open(filename) as f:
        txt = f.read()
    return txt

def write_file_section(*, filename=None):
    with open(filename) as f:
        txt = f.read()
    return txt

def search_file(*, filename=None):
    ''' with python regex '''
    with open(filename) as f:
        txt = f.read()
    return txt

def call_tool(args):
    logger.debug(f'called tool with {args}')
    ''' calls a single tool '''
    call_id = args['id']
    assert args['type'] == 'function'
    function = args['function']
    fn_name = function['name']
    arguments = json.loads(function['arguments'])
    
    fun = tools['fn_name']
    ret = fun(**arguments)
    '''
    for argname, arg in arguments.items():
        pass
    '''

    return {'id':str(uuid4()), 'type':'function_call', 'name':fn_name, 'arguments':arguments, 'call_id': call_id}


class Agent(Actor):
    def __init__(self):
        self.name = None
        self.sysprompt = ""
        self.parent = None


    def traj(self, prompt, sender):
        ''' starts with sysprompt + prompt
        continues with toolcall or answer
        again prompt
        ...
        '''
        tr = Traj(prompt)
        #db[nid()] = ret
        #set_trace()

        for i in range(1):
            prompt = tr.get_prompt()
            ret = completion(prompt)
            self.send(sender, {'type':'status', 'status': 'got completion'})

            choice = ret['choices'][0]
            message = choice['message']
            finish_reason = choice['finish_reason']
            if finish_reason == 'stop':
                status = 'got completion'
                tr.add_completion(ret)
                answer = {"type":'answer', 'content':message['content']}
                self.send(sender, answer)
                return
            elif finish_reason == 'tool_calls':
                status = 'got toolresult'
                assert 'tool_calls in message', message
                logger.debug(message)
                #tool = self.createActor(Tool)
                #self.send(tool, {'call':tc, 'tr':tr})
                res = []
                for tc in message['tool_calls']:
                    res.append(call_tool(tc))
                toolresult = json.dumps(res)
                #toolresult = json.dumps({"answer":"tool printed hello world"})
                tr.add_toolresult(toolresult)
                self.send(sender, 'calling tool')

            else:
                raise RuntimeError('unkown traj finish {choice}')


    def receiveMessage(self, message, sender):
        logger.info(f'agent got {message}')
        if isinstance(message, dict):
            assert 'type' in message
            t = message['type']
            if t == 'created':
                self.name = message['name']
                self.parent = sender
            elif t == "toolresult":
                #self.send(self.parent, {'type':'answer', 'content':f'agent got toolresult {message}'} )
                pass
            elif t == 'dm':
                message = message['value'].strip()

                if message == 'ping':
                    logger.info('got ping')
                    self.send(sender, f'pong from {self.name}')
                    return 

                prompt = message
                self.traj(prompt, sender)



class Chatroom(Actor):
    def __init__(self):
        self.agents = {}
        self.parent = None

    def new(self,s=snid()):
        if s not in self.agents:
            logger.info("starting agent")
            agent = self.createActor(Agent)
            self.send(agent, {"room": self.myAddress, "name": s, 'type':'created'})
            self.agents[s] = agent
            print(f'launched agent {s}')

    def receiveMessage(self, message, sender):
        logger.debug(f'room got {message}')
        if isinstance(message, dict):
            if message['type'] == 'init':
                self.new('default')
                self.parent = sender
                return 
            elif message['type'] == 'usermessage':
                ms = message['content'].strip()
                if ms.startswith('/'):
                    if ms.startswith('/new'):
                        s = ms.split()
                        if len(s) > 1:
                            self.new(s[1])
                        else:
                            self.new()

                    if ms.startswith('/ls'):
                        print(f'users: {self.agents}')

                    return
                elif ms.startswith('@') and len(ms) > 1:
                    ag = ms[1:].split()[0]
                    msg = ms.removeprefix(f'@{ag}')
                    if ag not in self.agents:
                        pass
                    else:
                        self.send(self.agents[ag], {'type':'dm', 'sender':'room', 'value':msg})

                    return

                ag = 'default'
                self.send(self.agents[ag], {'type':'dm', 'sender':'room', 'value':message['content']})
            elif message['type'] == 'answer':
                logger.info(f'room got answer: {message["content"]}')
                self.send(self.parent, message)
            else:
                print(f'ft {message}')
        else:
            print(f'room here {message}')


start_sequence = [
        '/new qwen3',
        '/new magistral'
        ]

def main():
    try:
        #room = ActorSystem("multiprocQueueBase").createActor(Chatroom)
        room = ActorSystem().createActor(Chatroom)
        num_agents = 1
        #answer = ActorSystem().ask(room, '/new', 1)
        answer = ActorSystem().ask(room, {'type':'init'}, 3)
        print(answer)

        while True:
            try:
                msg = {'type':'usermessage', "content": multiline_input("> ", 10)}
                #db.save({"type": "usermessage", "content": msg})
                #db[nid()] = {"type": "usermessage", "content": msg}
                #answer = ActorSystem().tell(room, msg)#, 1)
                answer = ActorSystem().ask(room, msg, 3)
                logging.info(f'main got answer: {answer}')
                print(answer)
            except KeyboardInterrupt:
                break

    finally:
        ActorSystem().tell(room, ActorExitRequest())

def test():
    try:
        #room = ActorSystem("multiprocQueueBase").createActor(Chatroom)
        room = ActorSystem().createActor(Chatroom)
        num_agents = 1
        #answer = ActorSystem().ask(room, '/new', 1)
        answer = ActorSystem().ask(room, {'type':'init'}, 3)
        print(answer)

        msg = {'type':'usermessage', "content": 'call the python tool'}
        #db.save({"type": "usermessage", "content": msg})
        #db[nid()] = {"type": "usermessage", "content": msg}
        #answer = ActorSystem().tell(room, msg)#, 1)
        answer = ActorSystem().ask(room, msg, 3)
        logging.info(f'main got answer: {answer}')
        print(answer)

    finally:
        ActorSystem().tell(room, ActorExitRequest())

def test2():
    prompt='use the python tool to run hello wold'
    tr = Traj(prompt)
    prompt = tr.get_prompt()
    ret = completion(prompt)
    print(ret)
    tr.add_prompt('continue')
    ret = completion(tr.get_prompt())


pool = mp.Pool(4)

if __name__ == '__main__':
    #main()
    test()
    #test2()
    pool.close()

