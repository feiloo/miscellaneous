from thespian.actors import *
from time import sleep
import json

import pycouchdb
import requests
import pydantic
import subprocess
import multiprocessing as mp
import time

username = ''
psw = ''
server = pycouchdb.Server(f"http://{username}:{psw}@localhost:5984/")
del username
del psw



db = server.database("messages2")


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

class Tool(Actor):
    def __init__(self):
        self.name = "python interpreter"

    def receiveMessage(self, message, sender):
        toolresult = "toolresult: python ran sucessfully and returned the number 2033"
        self.send(sender, toolresult)


def completion(prompt):
    #ret = requests.post("http://localhost:8082/completion", data=json.dumps(prompt))
    ret = requests.post("http://localhost:8082/v1/chat/completions", data=json.dumps(prompt))
    ret.raise_for_status()
    return ret.json()

tools = [
            {
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
                "required":["code"]
                }
            }
            }
        ]


class Agent(Actor):
    def __init__(self):
        self.name = None
        self.sysprompt = ""

    def receiveMessage(self, message, sender):
        print(message)
        #if sender == "tool":
        if message.strip().startswith("toolresult"):
            print(f'agent got toolresult {message}')
        if message.strip() == 'lol':
            print("lolrec")
        else:
            prompt = message
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
            prompt = {"messages":[
                {"role": "system", "content":sysprompt},
                {"role": "user", "content":prompt},
                ],
                      "tools": tools
                }

            try:
                ret = completion(prompt)
                db.save(ret)

                #print(f"prompt: {prompt} \nreply:\n {ret['content']} \n tools: {ret['tools']} \n\n")
                #print(ret['choices'][0]['message']['content'])
                print(json.dumps(ret,indent=4))

                message = ret['choices'][0]['message']
                if 'tool_calls' in message:
                    tool_calls = ['tool_calls']
                    tool = self.createActor(Tool)
                    self.send(tool, tool_calls)
                    print(tool_calls)
                    #self.send(sender, "ok "+str(ret))

            except Exception as e:
                print(e)
                pass


class Chatroom(Actor):
    def __init__(self):
        self.agents = {}

    def receiveMessage(self, message, sender):
        if isinstance(message, str):
            agent = self.createActor(Agent)
            self.agents[0] = agent
            self.send(self.agents[0], message)# self.myAddress)
            '''
            if message.strip().startswith('/start_agent'):
                agent = self.createActor(Agent)
                agent.send({"name": "1", "model": "qwen3"})
                self.users.append(agent)
            '''
            if message.strip().startswith('/ls'):
                print(self.users)


start_sequence = [
        '/start_agent qwen3',
        '/start_agent magistral'
        ]

def main():
    room = ActorSystem("multiprocQueueBase").createActor(Chatroom)
    num_agents = 1
    while True:
        try:
            msg = multiline_input("> ", 10)
            db.save({"type": "usermessage", "content": msg})
            #answer = ActorSystem().tell(room, msg)#, 1)
            answer = ActorSystem().ask(room, msg, 1)
            print(answer)
        except KeyboardInterrupt:
            break

    ActorSystem().tell(room, ActorExitRequest())


pool = mp.Pool(4)

if __name__ == '__main__':
    main()
    pool.close()

