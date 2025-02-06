from time import time
import json
import zmq
import argparse
import logging

import subprocess
import tempfile
import os

responder, requester = None, None

#zmq_addr = 'inproc:///tmp/assistant_zmq/0'
zmq_addr = 'ipc:///tmp/assistant_zmq/0'
#zmq_addr = 'inproc://assistant_zmq_0'
#zmq_addr = "tcp://localhost:5555"

def server_load():
    context = zmq.Context()
    responder = context.socket(zmq.REP)
    #responder.bind("tcp://*:5555")
    responder.bind(zmq_addr)
    return responder

def load_model():
    model_name = "Qwen/Qwen2.5-Coder-32B-Instruct"
    #model_name = "Qwen/QwQ-32B-Preview"

    model = AutoModelForCausalLM.from_pretrained(
                model_name,
                torch_dtype="auto",
                device_map="auto"
                )
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    # Initialize the DynamicCache
    past_key_values = DynamicCache()
    max_cache_length = past_key_values.get_max_length()
    return model, tokenizer, past_key_values, max_cache_length


def server():
    global responder
    if responder is None:
        responder = server_load()
    model, tokenizer, past_key_values, max_cache_length = load_model()

    while 1:
        request = json.loads(responder.recv_string())
        messages, max_new_tokens = request['messages'], request['max_new_tokens']
        text = tokenizer.apply_chat_template(
                    messages,
                    tokenize=False,
                    add_generation_prompt=True
                    )
        model_inputs = tokenizer([text], return_tensors="pt").to(model.device)

        if isinstance(past_key_values, SinkCache):
            inputs = {k: v[:, -max_cache_length:] for k, v in model_inputs.items()}

        generated_ids = model.generate(**model_inputs, max_new_tokens=32768, do_sample=False, past_key_values=past_key_values)
        generated_ids = [
                    output_ids[len(input_ids):] for input_ids, output_ids in zip(model_inputs.input_ids, generated_ids)
                    ]

        response = tokenizer.batch_decode(generated_ids, skip_special_tokens=True)[0]
        responder.send_string(json.dumps(response))


def client_load():
    context = zmq.Context()
    requester = context.socket(zmq.REQ)
    requester.connect(zmq_addr)
    return requester

def send_request(request):
    global requester
    if requester is None:
        requester = client_load()
    requester.send_string(json.dumps(request))
    reply = json.loads(requester.recv_string())
    return reply


# qwen

def edit_with_neovim(initial_content: str) -> str:
    # Create a temporary file
    with tempfile.NamedTemporaryFile(mode='w+', delete=False, suffix='.txt') as temp_file:
        temp_path = temp_file.name

        # Write the initial content to the temporary file
        temp_file.write(initial_content)
        temp_file.flush()

    try:
        # Open the temporary file in Neovim
        subprocess.run(['nvim', temp_path], check=True)

        # Read the edited content from the temporary file
        with open(temp_path, 'r') as temp_file:
            edited_content = temp_file.read()
    finally:
        # Clean up the temporary file
        os.remove(temp_path)

    return edited_content

#/ qwen

# actions : [initializing, waiting, inferring, feedback, quitting]
state = {'model':None, 'tokenizer':None, 'context':[], 'action':'initializing'} 

def action_upd(action, next_action):
    ''' functional state machine, to verify repl-state '''
    if action == 'initializing' and next_action == 'waiting':
        return next_action
    elif action == 'waiting' and next_action == 'waiting':
        return next_action
    elif action == 'inferring' and next_action == 'waiting':
        return next_action
    elif action == 'waiting' and next_action == 'inferring':
        return next_action
    elif action == 'inferring' and next_action == 'feedback':
        return next_action
    elif action == 'inferring' and next_action == 'initializing':
        return next_action
    elif action == 'feedback' and next_action == 'waiting':
        return next_action
    elif action == 'quitting':
        raise RuntimeError(state)
    elif next_action == 'quitting':
        return next_action
    else:
        raise RuntimeError(state)


def state_action_upd(state, n_act):
    state['action'] = action_upd(state['action'], n_act)
    return state


def efilter(*args, **kwargs):
    return list(filter(*args, **kwargs))

def dpop(k, dic):
    dic.pop(k, None)
    return dic

def filter_times(context):
    return [dpop('time', d) for d in context]



def infer(model, tokenizer, context, prompt):
    if context == []:
        context = [{"role": "system", "content": "You are Qwen, a helpful assistant."}]

    prompt_msg = {"role": "user", "content": prompt, 'time':time()}
    ctx_without_rew = efilter(lambda x: x['role'] != 'user_reward', context)
    messages = filter_times(context + [prompt_msg])
    max_new_tokens = 512
    response = send_request({'messages':messages, 'max_new_tokens':max_new_tokens})
    response_msg =  {"role": "assistant", "content":str(response), 'time':time()}
    state['context'] = context + [prompt_msg, response_msg]
    return state, response

def clear_(state):
    state['context'] = []
    resp = 'cleared'
    return state, resp


def save_(state):
    with open('qwen_db.jsonl','a') as f:
        s = state.copy()
        s.pop('model')
        s.pop('tokenizer')
        j = json.dumps(s)
        f.write(j + '\n')


def infer_control(state, prompt):
    p = prompt.strip()
    if p == 'edit':
        prompt_ = edit_with_neovim('')
        state, resp = infer(state['model'], state['tokenizer'], state['context'], prompt_)
    elif p == 'quit':
        save_(state)
        resp = 'quit'
    elif p == 'trace':
        resp = str(state)
    elif p == 'clear':
        state = clear_msg(state)
        save_(state)
        state, resp = clear_(state)
    else:
        state, resp = infer(state['model'], state['tokenizer'], state['context'], prompt)
    return state, resp

def infer_comb(state, prompt):
    return infer(state['model'], state['tokenizer'], state['context'], prompt)

def update_rew(state, reward):
    reward_msg = {"role": "user_reward", "content": reward, 'time':time()}
    state['context'] = state['context'] + [reward_msg]
    return state

def clear_msg(state):
    clear_msg_v = {"role": "user_clear", "content": [], 'time':time()}
    state['context'] = state['context'] + [clear_msg_v]
    return state

def multiline_input(p):
    contents = []
    while True:
        try:
            line = input(p)
        except EOFError:
            break
        contents.append(line)
    return '\n'.join(contents)




def repl(state):
    try:
        while 1:
            try:
                state = state_action_upd(state, 'waiting')
                p = multiline_input('> ')
                state = state_action_upd(state, 'inferring')
                state, resp = infer_control(state, p)
                print(resp)
                if resp == 'quit':
                    state = state_action_upd(state, 'quitting')
                    break
                elif resp == 'cleared':
                    state = state_action_upd(state, 'waiting')
                    continue
                state = state_action_upd(state, 'feedback')
                try:
                    rew = input('< ')
                except KeyboardInterrupt:
                    rew = ''
                except EOFError:
                    rew = ''
                state = update_rew(state, rew)
            except KeyboardInterrupt:
                state = state_action_upd(state, 'waiting')
                pass
    except:
        save_(state)
    return state




if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='inference api')
    subparsers = parser.add_subparsers(dest='mode', required=True)
    server_parser = subparsers.add_parser('server', help='run server')
    client_parser = subparsers.add_parser('prompt', help='make single prompt')
    client_parser.add_argument('prompt_text', help='the prompt to query the ai with')
    repl_parser = subparsers.add_parser('repl', help='multiple prompts in a repl-fashion')
    args = parser.parse_args()

    if args.mode == "server":
        # import hf here, since its slow to import and only used by the server
        from transformers import AutoModelForCausalLM, AutoTokenizer, SinkCache, DynamicCache
        print('starting server')
        server()
    elif args.mode == "prompt":
        print('starting client')
        #reply = send_request({'text':str(args.prompt_text), 'max_new_tokens':2048, 'messages':[]})
        model_name = "Qwen/Qwen2.5-Coder-32B-Instruct"
        model = None
        #tokenizer = AutoTokenizer.from_pretrained(model_name)
        tokenizer = None
        _, reply = infer(model, tokenizer, [], str(args.prompt_text))
        print(f'{reply}')
    elif args.mode == "repl":
        repl(state)
