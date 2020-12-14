# NOTES for inprog44
# inprog33 was test on the developer environment
# default vals of xdeets was NONE there, and on GAE it is null - that diff is throwing problems:
# added this to solve it: xdeets = ndb.StringProperty(default="None")

# cron job now works on GAE and print the reset entity to the GAE logging console


# this server does everything you need:
# 1) initializes the chain manager entity, called Chain
# ) adds a new participant to a slot in a chain or assigns them to the individual condition
# ) saves all participant data from the experiment - each round is an entity called roundData

# ) sets the maximum number of generations per chain

import webapp2
import logging
import random
from google.appengine.ext import ndb
import csv
import time # for getting current time on the system this code is running on (so GAE's machine I guess - but I see it in UTC)

# TO DO
# 1) make failsafe if all chains are busy

# STEP 1
# create several chain entities manually in the datastore (in project first_demo)
# https://console.cloud.google.com/datastore

################################################################################################
# SET THESE PARAMETERS

# number of chains to run in each frequency condition
U_chains = 15  # 15
R_chains = 15  # 15
L_chains = 15  # 15

# number of individuals to run in each frequency condition
U_individuals = 30  # 30 
R_individuals = 30  # 30
L_individuals = 30  # 30

# stop chain after you've collected the testing data from this generation:
max_generation = 30 # 20

# if a "busy" slot's last ping was over this many minutes ago, reset it to "available"
stale_tolerance = 5 # 5 minutes. (cron job runs \clear_stales every 5 min, js pings every 1 min)

# NOTES
# set equal numbers of participants per frequency condition for main batch
# and if you need to re-run a few later, you can do that piecemeal in a second batch - just change these params and re-deploy the experiment

################################################################################################

class Chain(ndb.Model):
	chain = ndb.IntegerProperty()
	generations_done = ndb.IntegerProperty()
	transmit = ndb.StringProperty() # this is the data to transmit from the last generation to the next (testing data -> training data)
	previous = ndb.StringProperty() # this is the "transmit data" from the previous participant. Chain has converged if previous = transmit.
	status = ndb.StringProperty() # 3 values: "available", "DONE", "busy: +timestamp"
	parent = ndb.StringProperty()
	distribution = ndb.StringProperty()
	xping = ndb.FloatProperty(default=0) # time the last ping was received. value = milliseconds since the epoch (1970) began.
	xdeets = ndb.StringProperty(default="None")
	created = ndb.StringProperty()

class Individual(ndb.Model):
	code = ndb.StringProperty() # completion_code, participant ID
	distribution = ndb.StringProperty()
	status = ndb.StringProperty()
	xping = ndb.FloatProperty(default=0) # time the last ping was received. value = milliseconds since the epoch (1970) began.
	xdeets = ndb.StringProperty(default="None")
	created = ndb.StringProperty()

# DELETE
# contains all of the data from one participant (will be row entry in master dataframe)
class participantData(ndb.Model):
	completion_code = ndb.StringProperty()
	chain = ndb.IntegerProperty()
	generation = ndb.IntegerProperty()
	data = ndb.StringProperty()
	created = ndb.StringProperty()

# every time a participant hits the bootout screen, saves their entityID, condition, and the time
class bootoutData(ndb.Model):
	entity_ID = ndb.StringProperty()
	condition = ndb.StringProperty()
	experiment = ndb.StringProperty() # (will be the same for ALL participants in this experiment)
	created = ndb.StringProperty()

# when a stale slot is detected and resent to "available", log that info in a staleSlot entity
class staleSlot(ndb.Model):
	session_code = ndb.StringProperty()
	condition = ndb.StringProperty()
	distribution = ndb.StringProperty()
	phase = ndb.StringProperty()
	created = ndb.StringProperty()

# contains all of the data from one participant (will be row entry in master dataframe)
# use column names that are identical to the experiment.js variable name, unless otherwise noted
class roundData(ndb.Model):
	participant = ndb.StringProperty() # js variable: completion_code
	session_code = ndb.StringProperty()
	condition = ndb.StringProperty()
	distribution = ndb.StringProperty() # js variable: frequency_condition
	chain = ndb.StringProperty()
	generation = ndb.StringProperty()
	parent = ndb.StringProperty()
	word0 = ndb.StringProperty()
	word1 = ndb.StringProperty()

	round = ndb.StringProperty()
	final_round = ndb.StringProperty()
	converged = ndb.StringProperty()
	correct_mapping = ndb.StringProperty()
	produced_mapping = ndb.StringProperty()
	score = ndb.StringProperty()
	
	trainset = ndb.StringProperty() # js variable: trainset_ID
	testset = ndb.StringProperty()  # js variable: testset_ID
	trainresponses = ndb.StringProperty()
	testresponses = ndb.StringProperty()
	trainresponse_changes = ndb.StringProperty()
	testresponse_changes = ndb.StringProperty()
	train_RTs = ndb.StringProperty()
	test_RTs = ndb.StringProperty()

	duration_consent = ndb.StringProperty()
	duration_callback = ndb.StringProperty()
	duration_comprehension = ndb.StringProperty()
	duration_instruction = ndb.StringProperty()
	duration_round = ndb.StringProperty()
	duration_experiment = ndb.StringProperty() # when final_round = true, this = whole experiment duration
	whole_experiment = ndb.StringProperty() # rounded to minutes (for easy visual check of whole experiment durations)

	jointime_webapp2 = ndb.StringProperty()
	jointime_user = ndb.StringProperty()
	jointime_user_UTC = ndb.StringProperty()
	bonused = ndb.StringProperty()

	# 5 removed:
	#round_begin = ndb.StringProperty()
	#round_end = ndb.StringProperty()
	#round_duration = ndb.StringProperty() # (millisecond length of the round)
	#experiment_duration = ndb.StringProperty() # (millisecond length of the experiment so far)
	#ip_address = ndb.StringProperty()

	comprehension_tries = ndb.StringProperty() # js variable: comprehension_tally
	experiment = ndb.StringProperty() # (will be the same for ALL participants in this experiment)
	created = ndb.StringProperty() # (this is generated in this server code - time round data written in UTC)

#######################################################
# IMPORTANT
# Only run the code in this section ONCE after you deploy the experiment! If you run it more than once, you need to go in to the datastore 
# and delete the second set of entities that it created.  Use the "created" property to sort them - that'll make it easy.
#
# Development location of datastore viewer:
# http://localhost:8000/datastore

class createChains(webapp2.RequestHandler):
	def get(self):

		# create all UNIFORM condition Chain entities 
		for i in range(1,U_chains+1):
			Chain(chain=i,distribution="U",generations_done=0,parent="init",previous="init",status="available",transmit="init",created=time.strftime('%X %x %Z')).put()

		# create all RIGHT SKEW condition Chain entities
		for i in range(1,R_chains+1):
			Chain(chain=i+U_chains,distribution="R",generations_done=0,parent="init",previous="init",status="available",transmit="init",created=time.strftime('%X %x %Z')).put()
		
		# create all LEFT SKEW condition Chain entities
		for i in range(1,L_chains+1):
			Chain(chain=i+U_chains+R_chains,distribution="L",generations_done=0,parent="init",previous="init",status="available",transmit="init",created=time.strftime('%X %x %Z')).put()

class createIndividuals(webapp2.RequestHandler):
	def get(self):

		# create all UNIFORM condition Individual entities 
		for i in range(1,U_individuals+1):
			rando = random.randint(100000,999999) # match to line in experiment.js: c_code = "c"+frequency_condition+nf(int(random(999999,100000)));
			c = "iU"+str(rando)
			Individual(code=c,distribution="U",status="available",created=time.strftime('%X %x %Z')).put()

		# create all RIGHT SKEW condition Individual entities 
		for i in range(1,R_individuals+1):
			rando = random.randint(100000,999999) # match to line in experiment.js: c_code = "c"+frequency_condition+nf(int(random(999999,100000)));
			c = "iR"+str(rando)
			Individual(code=c,distribution="R",status="available",created=time.strftime('%X %x %Z')).put()

		# create all LEFT SKEW condition Individual entities 
		for i in range(1,L_individuals+1):
			rando = random.randint(100000,999999) # match to line in experiment.js: c_code = "c"+frequency_condition+nf(int(random(999999,100000)));
			c = "iL"+str(rando)
			Individual(code=c,distribution="L",status="available",created=time.strftime('%X %x %Z')).put()

	# NOTE:
	# if a participant drops out or gets excluded, go to datastore viewer and manually re-set their status to "available" to run that slot again

#######################################################

class joinExperiment(webapp2.RequestHandler):

	def get(self):
		self.response.headers['Content-Type'] = 'text/plain'

		# assign participant to I or C condition
		cond = random.sample(["C","I"],1)[0]

		# check if there is a slot available in their condition

		# check if condition I
		if (cond == "I"):
			I_matches = Individual.query(Individual.status == "available").fetch()

			# if there is a slot in condition I, get the data for that slot
			if (len(I_matches) > 0):
				chosen = random.sample(I_matches,1)[0] # randomly select one of the matches (random.sample returns an array, so use [0] to pull the element out)
				chosen_id = chosen.key.id() # save the id of the chosen individual
				cod = chosen.code # save the entity's code value to a variable
				dis = chosen.distribution # save the entity's distribution value to a variable

				# write values to the datastore
				current_time = time.strftime('%X %x %Z')
				chosen.status = current_time # rewrite data entry as "busy"  # status = timestamp means status = "busy"
				chosen.xping = time.time() # write first ping time (in seconds), so xping never = 0.0 while status is "busy"
				chosen.put() # save that re-write to datastore

				# add the rest of the variables (since data string from both C and I must be formatted identically)
				cnd = cond
				chn = "NA"
				gen = "NA"
				trn = "NA"
				prn = "NA"

				logging.info("I condition slot assigned")

				# send values you got from the datastore to experiment.js
				self.response.write(str(chosen_id)+";"+str(cnd)+";"+str(cod)+";"+str(dis)+";"+str(chn)+";"+str(gen)+";"+str(trn)+";"+str(prn)+";"+str(current_time)) # use a unique separator symbol for js to parse

			# if there isn't  slot, try the other condition, C
			if (len(I_matches) == 0) :
				C_matches = Chain.query(Chain.status == "available").fetch()

				# if there is a slot in condition C, get the data for that slot
				if (len(C_matches) > 0):
					chosen = random.sample(C_matches,1)[0] # randomly select one of the matches (random.sample returns an array, so use [0] to pull the element out)
					chosen_id = chosen.key.id() # save the id of the chosen chain
					chn = chosen.chain # save the entity's chain value to a variable
					gen = chosen.generations_done # save the entity's generation value to a variable
					trn = chosen.transmit # save the entity's "data to transmit" to a variable
					prn = chosen.parent # save the entity's parent value to a variable
					dis = chosen.distribution # save the entity's distribution value to a variable
					
					# write values to the datastore
					current_time = time.strftime('%X %x %Z')
					chosen.status = current_time # rewrite data entry as "busy"  # status = timestamp means status = "busy"
					chosen.xping = time.time() # write first ping time (in seconds), so xping never = 0.0 while status is "busy"
					chosen.put() # save that re-write to datastore

					# add the rest of the variables (since data string from both C and I must be formatted identically)
					cnd = "C"  # change condition since we pulled a chain slot now
					cod = "NA"

					logging.info("C condition slot assigned")

					# send values you got from the datastore to experiment.js
					self.response.write(str(chosen_id)+";"+str(cnd)+";"+str(cod)+";"+str(dis)+";"+str(chn)+";"+str(gen)+";"+str(trn)+";"+str(prn)+";"+str(current_time))

			# and then if both conditions are full, don't do anything and the js code will handle it with an error message to the participant
			# (you can include any instructions for the participant in that error box)

		# check if condition C
		if (cond == "C"):
			C_matches = Chain.query(Chain.status == "available").fetch()

			# if there is a slot in condition C, get the data for that slot
			if (len(C_matches) > 0):
				chosen = random.sample(C_matches,1)[0] # randomly select one of the matches (random.sample returns an array, so use [0] to pull the element out)
				chosen_id = chosen.key.id() # save the id of the chosen chain
				chn = chosen.chain # save the entity's chain value to a variable
				gen = chosen.generations_done # save the entity's generation value to a variable
				trn = chosen.transmit # save the entity's "data to transmit" to a variable
				prn = chosen.parent # save the entity's parent value to a variable
				dis = chosen.distribution # save the entity's distribution value to a variable
				
				# write values to the datastore
				current_time = time.strftime('%X %x %Z')
				chosen.status = current_time # rewrite data entry as "busy"  # status = timestamp means status = "busy"
				chosen.xping = time.time() # write first ping time (in seconds), so xping never = 0.0 while status is "busy"
				chosen.put() # save that re-write to datastore

				# add the rest of the variables (since data string from both C and I must be formatted identically)
				cnd = cond
				cod = "NA"

				logging.info("C condition slot assigned")

				# send values you got from the datastore to experiment.js
				self.response.write(str(chosen_id)+";"+str(cnd)+";"+str(cod)+";"+str(dis)+";"+str(chn)+";"+str(gen)+";"+str(trn)+";"+str(prn)+";"+str(current_time))

			# if there isn't slot, try the other condition, I
			if (len(C_matches) == 0) :
				I_matches = Individual.query(Individual.status == "available").fetch()

				# if there is a slot in condition I, get the data for that slot
				if (len(I_matches) > 0):
					chosen = random.sample(I_matches,1)[0] # randomly select one of the matches (random.sample returns an array, so use [0] to pull the element out)
					chosen_id = chosen.key.id() # save the id of the chosen individual
					cod = chosen.code # save the entity's code value to a variable
					dis = chosen.distribution # save the entity's distribution value to a variable

					# write values to the datastore
					current_time = time.strftime('%X %x %Z')
					chosen.status = current_time # rewrite data entry as "busy"  # status = timestamp means status = "busy"
					chosen.xping = time.time() # write first ping time (in seconds), so xping never = 0.0 while status is "busy"
					chosen.put() # save that re-write to datastore

					# add the rest of the variables (since data string from both C and I must be formatted identically)
					cnd = "I"  # change condition since we pulled an individual slot now
					chn = "NA"
					gen = "NA"
					trn = "NA"
					prn = "NA"

					logging.info("I condition slot assigned")

					# send values you got from the datastore to experiment.js
					self.response.write(str(chosen_id)+";"+str(cnd)+";"+str(cod)+";"+str(dis)+";"+str(chn)+";"+str(gen)+";"+str(trn)+";"+str(prn)+";"+str(current_time))


class leaveChain(webapp2.RequestHandler):
	
	def post(self):
		# incoming data from experiment.js is formatted like this:
		# "chainID="+entityID+"c_code="+c_code+"transmit="+produced_mapping+"previous="+correct_mapping+"generation="+generation;

		# pull them all into local variables:
		chain_id = self.request.get('chainID')
		ccode = self.request.get('c_code')
		produced_mapping = self.request.get('transmit') # this testing set becomes the training set for the next participant
		previous_mapping = self.request.get('previous')
		gen = self.request.get('generation')
		#logging.info("previous_mapping: "+previous_mapping)
		#logging.info("produced_mapping: "+produced_mapping)

		# check if this participant caused the chain to converge or not
		il_converged = "false"
		logging.info(il_converged)
		if (produced_mapping == previous_mapping): # both of these variables are strings
			il_converged = "true"

		#logging.info(il_converged)

		# update the Chain entity
		entity = Chain.get_by_id(int(chain_id)) # get the correct chain entity by its id
		entity.parent = ccode
		entity.transmit = produced_mapping # write the new data into it
		entity.previous = previous_mapping
		entity.generations_done = int(gen) # generation of the participant whose data you will transmit next
		# (use value sent in from experiment, not the one already in Chain, in case multiple participants grabbed this slot. This'll guarantee the right testing data is associated with the right generation).
		
		if (int(gen) < max_generation):      # if we haven't hit the max generation yet
			if (il_converged == "false"):     # and if the chain has not converged
				entity.status = "available"  # update the status to available
			if (il_converged == "true"):    # but if the chain HAS converged
				entity.status = "DONE"       # stops further data collection for this chain
		if (int(gen) == max_generation): 
			entity.status = "DONE" # stops further data collection for this chain
		
		entity.put() # save the changes to the data store
		logging.info(entity)


class leaveIndividual(webapp2.RequestHandler):
	
	def post(self):
		# incoming data from experiment.js is formatted like this:
		# "individualID="+entityID;

		# pull variable into local variable:
		indiv_id = self.request.get('individualID')

		# update the Individual entity
		entity = Individual.get_by_id(int(indiv_id)) # get the correct entity by its id
		entity.status = "DONE"
		entity.put()
		logging.info(entity)


class abortChain(webapp2.RequestHandler):
	
	def post(self):
		# incoming data from experiment.js is formatted like this: "chainID="+entityID;

		# reset the entity's status to "available"
		chain_id = self.request.get('chainID')
		entity = Chain.get_by_id(int(chain_id))
		entity.status = "available"  # all other data in the chain entity is current and correct for the last generation - just reset status.
		entity.xping = 0
		entity.xdeets = "None"
		entity.put()
		logging.info(entity)

		# create a bootout entry so you can keep track of the number of participants lost to failing the comprehension questions
		b_data = bootoutData()
		b_data.entity_ID = chain_id
		b_data.condition = "C"
		b_data.experiment = self.request.get('experiment')
		b_data.created = time.strftime('%X %x %Z') # add the current time, to log the time that the data was written
		b_data.put() # then save it to the Datastore


class abortIndividual(webapp2.RequestHandler):
	
	def post(self):
		# incoming data from experiment.js is formatted like this: "individualID="+entityID;

		# reset the entity's status to "available"
		indiv_id = self.request.get('individualID')
		entity = Individual.get_by_id(int(indiv_id))
		entity.status = "available"
		entity.xping = 0
		entity.xdeets = "None"
		entity.put()
		logging.info(entity)

		# create a bootout entry so you can keep track of the number of participants lost to failing the comprehension questions
		b_data = bootoutData()
		b_data.entity_ID = indiv_id
		b_data.condition = "I"
		b_data.experiment = self.request.get('experiment')
		b_data.created = time.strftime('%X %x %Z') # add the current time, to log the time that the data was written
		b_data.put() # then save it to the Datastore


class saveRound(webapp2.RequestHandler):

	def post(self):
		# create a roundData() entity right here
		r_data = roundData()

		# then fill it with all the incoming round data from the experiment
		# everything on the right-hand side are the experiment.js variable names
		r_data.participant = self.request.get('c_code')
		r_data.session_code = self.request.get('session_code')
		r_data.condition = self.request.get('condition')
		r_data.distribution = self.request.get('frequency_condition')
		r_data.chain = self.request.get('chain')
		r_data.generation = self.request.get('generation')
		r_data.parent = self.request.get('parent')
		r_data.word0 = self.request.get('word0')
		r_data.word1 = self.request.get('word1')

		r_data.round = self.request.get('round_counter')
		r_data.final_round = self.request.get('final_round')
		r_data.converged = self.request.get('converged')
		r_data.correct_mapping = self.request.get('correct_mapping')
		r_data.produced_mapping = self.request.get('produced_mapping')
		r_data.score = self.request.get('score')

		r_data.trainset = self.request.get('trainset_ID')
		r_data.testset = self.request.get('testset_ID')
		r_data.trainresponses = self.request.get('trainresponses')
		r_data.testresponses = self.request.get('testresponses')
		r_data.trainresponse_changes = self.request.get('trainresponse_changes')
		r_data.testresponse_changes = self.request.get('testresponse_changes')
		r_data.train_RTs = self.request.get('train_RTs')
		r_data.test_RTs = self.request.get('test_RTs')

		r_data.duration_consent = self.request.get('duration_consent')
		r_data.duration_callback = self.request.get('duration_callback')
		r_data.duration_comprehension = self.request.get('duration_comprehension')
		r_data.duration_instruction = self.request.get('duration_instruction')
		r_data.duration_round = self.request.get('duration_round')
		r_data.duration_experiment = self.request.get('duration_experiment')
		r_data.whole_experiment = self.request.get('whole_experiment')

		r_data.jointime_webapp2 = self.request.get('jointime_webapp2')
		r_data.jointime_user = self.request.get('jointime_user')
		r_data.jointime_user_UTC = self.request.get('jointime_user_UTC')

		r_data.comprehension_tries = self.request.get('comprehension_tally')
		r_data.experiment = self.request.get('experiment')

		# add the current time, to log the time that the data was written
		r_data.created = time.strftime('%X %x %Z') 
		r_data.bonused = "no"

		# then save it to the Datastore
		r_data.put()
		logging.info(r_data)


class downloadRoundData(webapp2.RequestHandler):

	def get(self):
		# these headers tell the browser to expect a csv and do whatever it's default procedure is for csvs (Chrome: auto download)
		self.response.headers['Content-Type'] = 'text/csv' 
		self.response.headers['Content-Disposition'] = 'inline;filename=results.csv'
		
		# get every entity of the type roundData
		data = roundData.query().fetch()

		# Create the header that will contain the csv's column names, in the order you want.  
		# Each one must be a property name from the roundData entity (copy and paste here - must be identical).
		column_names = ['participant','session_code','condition','distribution','chain','generation','parent','word0','word1','round','final_round','converged','correct_mapping','produced_mapping','score','trainset','testset','trainresponses','testresponses','trainresponse_changes','testresponse_changes','train_RTs','test_RTs','duration_consent','duration_callback','duration_comprehension','duration_instruction','duration_round','duration_experiment','whole_experiment','jointime_webapp2','jointime_user','jointime_user_UTC','comprehension_tries','experiment','created','bonused']

		writer = csv.DictWriter(self.response.out, fieldnames=column_names) # use property names as the column names
		writer.writeheader() # write the row of column names in the csv

		# now fill the csv with one entity per row
		for e in data:
			d = dict()
			try:
				for k, v in e._properties.iteritems():
					d[k] = str(v._get_user_value(e))
				writer.writerow(d)
			except UnicodeEncodeError:
				logging.error("UnicodeEncodeError detected, row ignored");

class acceptVitals(webapp2.RequestHandler):

	def post(self):
		# incoming data from experiment.js is formatted like this: "entityID="+entityID+"&session_code="+session_code+"&condition="+condition+"&frequency_condition="+frequency_condition+"&phase="+phase;

		cond = self.request.get('condition')
		session = self.request.get('session_code')
		dist = self.request.get('frequency_condition')
		phase = self.request.get('phase')

		if (cond == "C") :
			chain_id = self.request.get('entityID')
			entity = Chain.get_by_id(int(chain_id)) # get the Chain entity
			entity.xping = time.time() # write ping info to entity (in seconds)
			entity.xdeets = session+";"+cond+";"+dist+";"+phase
			entity.put()
			logging.info(entity)

		if (cond == "I") :
			indiv_id = self.request.get('entityID')
			entity = Individual.get_by_id(int(indiv_id)) # get the Individual entity
			entity.xping = time.time() # write ping info to entity (in seconds)
			entity.xdeets = session+";"+cond+";"+dist+";"+phase
			entity.put()
			logging.info(entity)
		
class clearStales(webapp2.RequestHandler):

	def get(self):

		now = time.time() # save time that clearStales was called (in seconds)

		# get all Individual entities that are not "available" or "DONE"
		I_matches = Individual.query(ndb.AND(Individual.status != "available", Individual.status != "DONE")).fetch()

		# for all the occupied Individual slots:
		for i in (I_matches):
				
			# if this slot is officially stale 
			if ( now-i.xping > 60*stale_tolerance ):  # (now-i.xping = oldness in seconds) (60 sec * stale_tolerance min)

				# print the entity that was reset to available to the GAE console log (need to print now before the reset is made)
				logging.info(i)

				deets = i.xdeets  # save the deets before you reset them (it's possible to quit experiment while deets still = None)

				# then reset those that slot's status to "available" and reset xping and xdeets too.
				i.status = "available"
				i.xdeets = "None"
				i.xping = 0.0
				i.put()

				# and log the fact that it was reset
				s_slot = staleSlot()

				if (deets != "None"):
					deets_split = deets.split(";")

					s_slot.session_code = deets_split[0]
					s_slot.condition = deets_split[1]
					s_slot.distribution = deets_split[2]
					s_slot.phase = deets_split[3]
					s_slot.created = time.strftime('%X %x %Z')
					s_slot.put()

				if (deets == "None"):
					s_slot.session_code = "unknown"
					s_slot.condition = "I"
					s_slot.distribution = i.distribution
					s_slot.phase = "0" # not sure when participant left experiment, so 0 here means "before first real ping was made"
					s_slot.created = time.strftime('%X %x %Z')
					s_slot.put()

		# get all Chain entities that are not "available" or "DONE"
		C_matches = Chain.query(ndb.AND(Chain.status != "available", Chain.status != "DONE")).fetch()

		# for all the occupied Individual slots:
		for c in (C_matches):
				
			# if this slot is officially stale 
			if ( now-c.xping > 60*stale_tolerance ):  # (now-i.xping = oldness in seconds) (60 sec * stale_tolerance min)

				# print the entity that was reset to available to the GAE console log (need to print now before the reset is made)
				logging.info(c)

				deets = c.xdeets  # save the deets before you reset them (it's possible to quit experiment while deets still = None)

				# then reset those that slot's status to "available" and reset xping and xdeets too.
				c.status = "available"
				c.xdeets = "None"
				c.xping = 0.0
				c.put()

				# and log the fact that it was reset
				s_slot = staleSlot()

				if (deets != "None"):
					deets_split = deets.split(";")

					s_slot.session_code = deets_split[0]
					s_slot.condition = deets_split[1]
					s_slot.distribution = deets_split[2]
					s_slot.phase = deets_split[3]
					s_slot.created = time.strftime('%X %x %Z')
					s_slot.put()

				if (deets == "None"):
					s_slot.session_code = "unknown"
					s_slot.condition = "I"
					s_slot.distribution = i.distribution
					s_slot.phase = "0" # not sure when participant left experiment, so 0 here means "before first real ping was made"
					s_slot.created = time.strftime('%X %x %Z')
					s_slot.put()


app = webapp2.WSGIApplication([
	('/initialize_chains', createChains),
	('/initialize_individuals', createIndividuals),
	('/join_experiment',joinExperiment),
	('/leave_chain', leaveChain),
	('/leave_individual', leaveIndividual),
	('/abort_chain', abortChain),
	('/abort_individual', abortIndividual),
	('/send_vitals', acceptVitals),
	('/clear_stales', clearStales),
	('/save_round_data', saveRound),
	('/download_round_data_csv', downloadRoundData) # security risk?  can anyone use this url to download the data?
], debug=True)
