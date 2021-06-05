import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import networkx as nx
from networkx import read_edgelist
from math import exp
import random


lazega=read_edgelist('lazega-friends.edges',nodetype=int) 
node_atts=pd.read_csv("lazega-attributes.txt", sep=' ')


for i in range(node_atts.shape[0]):
    lazega.add_node(node_atts.loc[i,'nodeID'], gender=node_atts.loc[i,'nodeGender'])
    lazega.add_node(node_atts.loc[i,'nodeID'], office=node_atts.loc[i,'nodeOffice'])
    lazega.add_node(node_atts.loc[i,'nodeID'], true_practice=node_atts.loc[i,'nodePractice'])


random.seed(6)

for i in range(node_atts.shape[0]):
    if random.random() > 0.4:
        lazega.add_node(node_atts.loc[i,'nodeID'], observed_practice=node_atts.loc[i,'nodePractice'])
    else:
        lazega.add_node(node_atts.loc[i,'nodeID'], observed_practice=np.nan)


def get_att_array(G,att_name):
    ret_array=np.zeros(nx.number_of_nodes(G))
    for i,n in enumerate(G.nodes()):
        ret_array[i]=G.nodes[n][att_name]
    return(ret_array)


def init_practice():
    # This function should add an additional attribute predicted_practice.
    # The value of predicted_practices is the same as observed_practice when observed_practice != nan. Otherwise, 
    # predicted_practice is a randomly sampled 'practice' value.
    for node_id in lazega.nodes():
        n    = lazega.nodes[node_id]

        pred = random.random() > 0.5 if n['observed_practice'] is np.nan else n['observed_practice'] == 1

        n.update({'predicted_practice': 1 if pred else 2,
                  'p1'                : 1 if pred else 0,
                  'samples1'          : 1 if pred else 0,
                  'samples2'          : 0 if pred else 1})


def n_log_potential_1(n,w1,w2):
    if n['gender'] == 1: return w1
    else: return w2

def n_log_potential_2(n1,n2,w1,w2):
    if n1['predicted_practice']==n2['predicted_practice']: return w1
    else: return w2


def gibbs_sample(node_id, w1, w2, w3, w4, burn_in):
    # Iterate over all log-potential functions you want to use
    # For node potentials, evaluate the potential for the given node n
    # For edge potentials, evaluate the potential for all pairs (n,n') where n' is a neighbor in the 'friendship' graph.
    #
    # Since the friendship graph is directed, there are three possibilities of how to do this precisely:
    #        - consider all n' where friends(n,n')
    #        - consider all n' where friends(n',n)
    #        - consider both cases of n'
    # 
    # The method nx.all_neighbors(lazega,n) will return an iterator over both types of neighbors of n, so the third
    # option is the most convenient to use (and maybe also the most sensible)
    #
    # Sum the values of all the potential functions, and take the exponential.
    # This has to be done for both possible values of the 'predicted_practice' value for n
    #
    # You will probably need to add arguments to the gibbs_sample method for the numerical parameters of 
    # the potential functions that you are using.
    # 
    # Calculate the probability for predicted_practice(n) according to the quotient shown on slide 12 (see also slide 23)
    #
    # Set the new value of predicted_practice(n) randomly according to the probabilities you have just computed. 
    
    n = lazega.nodes[node_id]

    n.update({'predicted_practice': 1})

    log_pot_prod1 = n_log_potential_1(n, w1, w2)

    for node_id2 in nx.all_neighbors(lazega, node_id):
        log_pot_prod1 += n_log_potential_2(n, lazega.nodes[node_id2], w3, w4)

    pot_prod1 = exp(log_pot_prod1)

    n.update({'predicted_practice': 2})
    
    log_pot_prod2 = n_log_potential_1(n, w1, w2)

    for node_id2 in nx.all_neighbors(lazega, node_id):
        log_pot_prod2 += n_log_potential_2(n, lazega.nodes[node_id2], w3, w4)

    pot_prod2 = exp(log_pot_prod2)

    pot_prod_sum = pot_prod1 + pot_prod2
    p1           = pot_prod1 / pot_prod_sum

    if burn_in:
        if random.random() <= p1:
            n.update({'predicted_practice': 1,
                      'p1'                : 1,
                      'samples1'          : 1,
                      'samples2'          : 0})
        else:
            n.update({'predicted_practice': 2,
                      'p1'                : 0,
                      'samples1'          : 0,
                      'samples2'          : 1})
    else:
        if random.random() <= p1:
            n.update({'predicted_practice': 1,
                      'p1'                : (n['samples1'] + 1) / (n['samples1'] + n['samples2'] + 1),
                      'samples1'          : n['samples1'] + 1})
        else:
            n.update({'predicted_practice': 2,
                      'p1'                : n['samples1'] / (n['samples1'] + n['samples2'] + 1),
                      'samples2'          : n['samples2'] + 1})


def gibbs_one_round(G, burn_in):
    # Iterate over all nodes n in G for which observed_practice == nan, and re-sample its predicted_practice value

    for node_id in lazega.nodes():
        if lazega.nodes[node_id]['observed_practice'] is np.nan:
            gibbs_sample(node_id, 2, -1, 5, -3, burn_in)


# Initialize with init_att
#
# Perform a number of gibbs_one_round(lazega) sampling steps
#
# (Maybe after a certain number of burn-in iterations): keep count of how often the predicted_practice value of nodes 
# with observed_practice == nan is in the two states 1 or 2.
# 
# Predict the unobserved practice values as the more probably state in the Gibbs sample
#
# Compare your predicted values against the true values

# initialize
init_practice()

# burn in
for i in range(0, 20):
    gibbs_one_round(lazega, True)

for i in range(0, 1000):
    gibbs_one_round(lazega, False)

acc_list = []
pra_list = []
lab_dic = {}
n_corr = 0
n_samp = 0

for node_id in lazega.nodes():
    n = lazega.nodes[node_id]
    corr_pred = n['observed_practice'] is np.nan
    pra_list.append(400 if n['observed_practice'] is np.nan else 200)
    n.update({'observed_practice': 1 if n['p1'] >= 0.5 else 2})
    acc_list.append("#34eb40" if n['observed_practice'] == n['true_practice'] else "#eb4034")
    lab_dic[node_id] = f"{round(n['p1'], 2)}"
    n_corr += 1 if corr_pred and n['observed_practice'] == n['true_practice'] else 0
    n_samp += 1 if corr_pred else 0

nx.draw_kamada_kawai(lazega,
                     with_labels=True,
                     node_color=acc_list,
                     node_size=pra_list,
                     labels=lab_dic,
                     label=f"accuracy = {round(n_corr / n_samp, 2)}")

plt.legend(loc="upper left")
plt.show()