{-|
This data-flow framework consists mainly of 'Neuron's which are data processing units in data-flow network, receiving and sending
'Impulse's over bidirectional 'Nerve's 'attach'ed to each other. 'Neuron's and 'Nerve's are best 'grow'n in 'Incubation' monad, which takes care of
proper 'grow'ing and 'dissolve'-ing of 'Neuron's. It comes with some example 'Neuron's but you should probably define your own.
-}

module Control.Etage (
  module Control.Etage.Incubator,
  module Control.Etage.Externals
) where

import Control.Etage.Externals
import Control.Etage.Incubator
