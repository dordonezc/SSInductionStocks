## Random Walk
import numpy as np 
import TransitionMatrix as tm
import ipdb

def run_random_walk(M, teleport, beta, **kwargs):
    """
    Calls the iteration scheme. Sets the pulling strength to seed set
    """
    #ipdb.set_trace()
    def update_seeds(r):
        r += (1 - beta) * teleport / np.sum(teleport)
    return run_iterative(M * beta, np.ones(M.shape[1]) / M.shape[1], update_seeds, **kwargs)

def random_walk(words, embeddings, positive_seeds, negative_seeds, beta=0.9, **kwargs):
    """
    Learns polarity scores via random walks with teleporation to seed sets.
    Main method used in paper. 
    """
    #ipdb.set_trace()
    if not type(positive_seeds) is dict:
        positive_seeds = {word:1.0 for word in positive_seeds}
        negative_seeds = {word:1.0 for word in negative_seeds}
    M = tm.transition_matrix(embeddings, **kwargs)
    rpos = run_random_walk(M, weighted_teleport_set(words, positive_seeds), beta, **kwargs)
    rneg = run_random_walk(M, weighted_teleport_set(words, negative_seeds), beta, **kwargs)
    ret = ({w: rpos[i] / (rpos[i] + rneg[i]) for i, w in enumerate(words)},
          {w: rneg[i] / (rpos[i] + rneg[i]) for i, w in enumerate(words)})
    return ret


def run_iterative(M, r, update_seeds, max_iter=50, epsilon=1e-6, **kwargs):
    """
    Random walk with 50 steps or until convergence. Iterates over transition matrix
    gets pulled back to seed set. 
    """
    #ipdb.set_trace()
    for i in range(max_iter):
        last_r = np.array(r)
        r = np.dot(M, r)
        update_seeds(r)
        if np.abs(r - last_r).sum() < epsilon:
            break
    return r

def weighted_teleport_set(words, seed_weights):
    """
    Initial value for seed set. Basically 1 if the word is in seed set.
    """
    aux = [seed_weights[word] if word in seed_weights else 0.0 for word in words]
    return np.array(aux)

