"""
Code received from https://github.com/dhruvp/atari-pong/blob/master/me_pong.py

This is being used to allow quick and easy creation of the up.
I look to build my own implementation in the future.
"""


## Architecture

# Take in inputs from the screen and preprocess them
# Pass them into an NN
# Update the weights of the NN using gradient descent
# weights['1'] - Matrix that holds weights of pixels passing into hidden layer.
# Dimensions: [200 x 80 x 80] -> [200 x 6400]
# weights['2'] - Matrix that holds weights of hidden layer passing into output.
# Dimensions: [1 x 200]

# Process is:

# processed_observations = image vector - [6400 x 1] array
# Compute hidden_layer_values = weights['1'] dot processed_observations 
# ([200 x 6400] dot [6400 x 1]) -> [200 x 1]
# This gives initial activation values.
# Next we need to transform those either via a sigmoid or an ReLU of some sort.
# Let's use ReLU
# ReLU(hidden_layer_values)
# Next we need to pass this one layer further
# output_layer_value = weights['2'] dot hidden_layer_values
# ([1 x 200] dot [200 x 1] -> [1 x 1])
# Now our output layer is the probability of going up or down.
# Let's make sure this output is between 0 and 1 by passing it through a sigmoid
# p = sigmoid(output_layer_value)

# Learning after round has finished:

# Figure out the result
# Compute the error
# Use the error to calculate the gradient
# The below dimensions all assume we had exactly 10 frames in the round
# (not necessarily true!)
# dC_dw2 = hidden_layer_values^T dot gradient_log_p
# ([1 x 2000] dot [2000 x 1] -> 1x1)
# delta_1 = gradient_log_p outer_product weights['2'] = [2000 x 1]
#                          outer_product [1 x 200] ([2000 x 200])
# dC_dw1 = delta_1^T dot input_observations
# ([200 x 2000]x dot [2000 x 64000] -> [200 x 64000])
# After some batch size of rounds has finished,
# Use rmsprop to move weights['1'] and weights['2'] in 
# the direction of the gradient
# Repeat!

import gym
import numpy as np
from functools import reduce


PARAMETERS = {
    'batch_size': 10
    , 'gamma': 0.99 # discount factor for reward
    , 'decay_rate': 0.99
    , 'num_hidden_layer_neurons': 200
    , 'input_dimensions': 80 * 80
    , 'learning_rate': 1e-4
    , 'episode_number': 0
    , 'reward_sum': 0
    , 'running_reward': None
    , 'prev_processed_observations': None
    , 'episode_hidden_layer_values': []
    , 'episode_observations': []
    , 'episode_gradient_log_ps': []
    , 'episode_rewards': []
    }


WEIGHTS = {
    '1': np.random.randn(PARAMETERS['num_hidden_layer_neurons'], PARAMETERS['input_dimensions']) / np.sqrt(PARAMETERS['input_dimensions']),
    '2': np.random.randn(PARAMETERS['num_hidden_layer_neurons']) / np.sqrt(PARAMETERS['num_hidden_layer_neurons'])
    }


def randomize_weights(hidden_layers, input_dim):
    weight_1 = np.random.randn(hidden_layers, input_dim) / np.sqrt(input_dim)
    weight_2 = np.random.randn(hidden_layers) / np.sqrt(hidden_layers)
    return weight_1, weight_2


def create_g_dict(weight_1, weight_2):
    return {
        '1' : np.zeros_like(weight_1)
        , '2': np.zeros_like(weight_2)
    }


def init_model(hidden_layers, input_dim):
    weight_1, weight_2 = randomize_weights(hidden_layers, input_dim)
    g_initial = create_g_dict(weight_1, weight_2)
    all_parameters = {
        'batch_size': 10
        , 'epsilon': 1e-5
        , 'gamma': 0.99 # discount factor for reward
        , 'decay_rate': 0.99
        , 'num_hidden_layer_neurons': hidden_layers # 200
        , 'input_dimensions': input_dim # 80 * 80
        , 'learning_rate': 1e-4
        , 'episode_number': 0
        , 'reward_sum': 0
        , 'running_reward': None
        , 'prev_processed_observation': None
        #, 'weight_1': weight_1
        #, 'weight_2': weight_2
        , 'weights': {
            '1': weight_1
            , '2': weight_2
            }
        , 'episode_hidden_layer_values': []
        , 'episode_observations': []
        , 'episode_gradient_log_ps': []
        , 'episode_rewards': []
        , 'expectation_g_squared': g_initial
        , 'g_dict': g_initial
        }

    return all_parameters



def downsample(image):
    # Take only alternate pixels - basically halves the resolution of the image (which is fine for us)
    return image[::2, ::2, :]


def remove_color(image):
    """Convert all color (RGB is the third dimension in the image)"""
    return image[:, :, 0]


def remove_background(image):
    image[image == 144] = 0
    image[image == 109] = 0
    return image

def pipe(pipeline, initial_arg):
    return reduce(lambda x, y: y(x), pipeline, initial_arg)


def preprocess_observations(input_observation, model):
    """ convert the 210x160x3 uint8 frame into a 6400 float vector """
    cropped_observation = input_observation[35:195] # crop
    processed_observation = pipe([downsample, remove_color, remove_background]
                                 , cropped_observation)
    # everything else (paddles, ball) just set to 1
    processed_observation[processed_observation != 0] = 1 
    # Convert from 80 x 80 matrix to 1600 x 1 matrix
    processed_observation = processed_observation.astype(np.float).ravel()

    # subtract the previous frame from the current one so
    # we are only processing on changes in the game
    if model['prev_processed_observation'] is not None:
        input_observation = ( processed_observation
                            - model['prev_processed_observation'] )
    else:
        input_observation = np.zeros(model['input_dimensions'])
    # store the previous frame so we can subtract from it next time
    prev_processed_observations = processed_observation
    return input_observation, prev_processed_observations


def sigmoid(x):
    return 1.0/(1.0 + np.exp(-x))


def relu(vector):
    vector[vector < 0] = 0
    return vector


def apply_neural_nets(observation_matrix, weights):
    """ Based on the observation_matrix and weights, compute the new hidden layer values and the new output layer values"""
    hidden_layer_values = np.dot(weights['1'], observation_matrix)
    hidden_layer_values = relu(hidden_layer_values)
    output_layer_values = np.dot(hidden_layer_values, weights['2'])
    output_layer_values = sigmoid(output_layer_values)
    return hidden_layer_values, output_layer_values


def choose_action(probability):
    random_value = np.random.uniform()
    if random_value < probability:
        # signifies up in openai gym
        return 2
    else:
         # signifies down in openai gym
        return 3


def compute_gradient(gradient_log_p, model):
    """ See here: http://neuralnetworksanddeeplearning.com/chap2.html"""
    delta_L = gradient_log_p
    dC_dw2 = np.dot(model['episode_hidden_layer_values'].T, delta_L).ravel()
    delta_l2 = np.outer(delta_L, model['weights']['2'])
    delta_l2_relu = relu(delta_l2)
    dC_dw1 = np.dot(delta_l2_relu.T, model['episode_observations'])
    return {
        '1': dC_dw1,
        '2': dC_dw2
    }


def update_weights(model):
    """
    See here:
        http://sebastianruder.com/optimizing-gradient-descent/index.html#rmsprop
    """
    for layer_name in model['weights']:
        g = model['g_dict'][layer_name]
        updated_layer = ( model['decay_rate'] 
                         * model['expectation_g_squared'][layer_name]
                         + (1 - model['decay_rate']) * g**2 )

        model['expectation_g_squared'][layer_name] = updated_layer
        update_weights = ( ( model['learning_rate'] * g)
                          / (np.sqrt(model['expectation_g_squared'][layer_name]
                            + model['epsilon'])))

        model['weights'][layer_name] += update_weights
        reset_gradient = np.zeros_like(model['weights'][layer_name])
        model['g_dict'][layer_name] = reset_gradient

    return model


def discount_rewards(rewards, gamma):
    """
    Actions you took 20 steps before the end result are less important to
    the overall result than an action you took a step ago.
    This implements that logic by discounting the reward on previous
    actions based on how long ago they were taken
    """
    discounted_rewards = np.zeros_like(rewards)
    running_add = 0
    for t in reversed(xrange(0, rewards.size)):
        if rewards[t] != 0:
            # reset the sum, since this was a game boundary (pong specific!)
            running_add = 0
        running_add = running_add * gamma + rewards[t]
        discounted_rewards[t] = running_add
    return discounted_rewards


def discount_with_rewards(model):
    """ discount the gradient with the normalized rewards """
    dsct_rewards = discount_rewards(model['episode_rewards'], model['gamma'])
    # standardize the rewards to be unit normal
    # (helps control the gradient estimator variance)
    dsct_rewards -= np.mean(dsct_rewards)
    dsct_rewards /= np.std(dsct_rewards)
    return model['episode_gradient_log_ps'] * dsct_rewards


def initiate_environment(env_name):
    env = gym.make(env_name)
    observation = env.reset()
    return env, observation

def main():
    # To be used with rmsprop algorithm
    # (http://sebastianruder.com/optimizing-gradient-descent/index.html#rmsprop)
    env, observation = initiate_environment("Pong-v0")
    #rmsprop_vars = initiate_variables()
    #g_const = initiate_g_dict(WEIGHTS)
    model = init_model(200, 80 * 80)
    while True:
        env.render()
        processed_obs, new_obs = preprocess_observations(observation, model)
        model['prev_processed_observations'] = new_obs
        hidden_layer_values, up_prob = apply_neural_nets(processed_obs
                                                         , model['weights'])
 
        model['episode_observations'].append(processed_obs)
        model['episode_hidden_layer_values'].append(hidden_layer_values)

        action = choose_action(up_prob)

        # carry out the chosen action
        observation, reward, done, info = env.step(action)

        model['reward_sum'] += reward
        model['episode_rewards'].append(reward)

        # see here: http://cs231n.github.io/neural-networks-2/#losses
        fake_label = 1 if action == 2 else 0
        loss_function_gradient = fake_label - up_prob
        model['episode_gradient_log_ps'].append(loss_function_gradient)


        if done: # an episode finished
            model['episode_number'] += 1

            # Combine the following values for the episode
            model['episode_hidden_layer_values'] = \
                        np.vstack(model['episode_hidden_layer_values'])
            model['episode_observations'] = \
                        np.vstack(model['episode_observations'])
            model['episode_gradient_log_ps'] = \
                        np.vstack(model['episode_gradient_log_ps'])
            model['episode_rewards'] = np.vstack(model['episode_rewards'])

            # Tweak the gradient of the log_ps based on the discounted rewards
            episode_gradient_log_ps_discounted = discount_with_rewards(model)


            gradient = compute_gradient(episode_gradient_log_ps_discounted
                                        , model)

            # Sum the gradient for use when we hit the batch size
            for weight in model['weights'].keys():
                model['g_dict'][weight] += gradient[weight]

            if model['episode_number'] % model['batch_size'] == 0:
                update_weights(model) #

            model['episode_hidden_layer_values'] = []
            model['episode_observations'] = []
            model['episode_gradient_log_ps'] = []
            model['episode_rewards'] = []
            observation = env.reset() # reset env
            model['running_reward'] = ( model['reward_sum']
                                       if model['running_reward'] is None
                                       else ( model['running_reward'] * 0.99
                                            + model['reward_sum'] * 0.01 ) )

            print('resetting env. episode reward total was {}. running mean: {}'
                  .format(model['reward_sum'], model['running_reward']))

            model['reward_sum'] = 0
            model['prev_processed_observations'] = None

if __name__ == '__main__':
    main()
