<?php
/*
 * Bacula(R) - The Network Backup Solution
 * Baculum   - Bacula web interface
 *
 * Copyright (C) 2013-2016 Kern Sibbald
 *
 * The main author of Baculum is Marcin Haba.
 * The original author of Bacula is Kern Sibbald, with contributions
 * from many others, a complete list can be found in the file AUTHORS.
 *
 * You may use this file and others of this release according to the
 * license defined in the LICENSE file, which includes the Affero General
 * Public License, v3.0 ("AGPLv3") and some additional permissions and
 * terms pursuant to its AGPLv3 Section 7.
 *
 * This notice must be preserved when any source code is
 * conveyed and/or propagated.
 *
 * Bacula(R) is a registered trademark of Kern Sibbald.
 */
 
class Pool extends BaculumAPI {
	public function get() {
		$poolid = intval($this->Request['id']);
		$pool = $this->getModule('pool')->getPoolById($poolid);
		$allowedPools = $this->getModule('bconsole')->bconsoleCommand($this->director, array('.pool'), $this->user);
		if ($allowedPools->exitcode === 0) {
			if(!is_null($pool) && in_array($pool->name, $allowedPools->output)) {
				$this->output = $pool;
				$this->error = PoolError::ERROR_NO_ERRORS;
			} else {
				$this->output = PoolError::MSG_ERROR_POOL_DOES_NOT_EXISTS;
				$this->error = PoolError::ERROR_POOL_DOES_NOT_EXISTS;
			}
		} else {
			$this->output = $allowedPools->output;
			$this->error = $allowedPools->exitcode;
		}
	}
	
	public function set($id, $params) {
		$result = ($this->user === null) ? $this->getModule('pool')->setPool($id, $params) : true;
		if($result === true) {
			$this->output = null;
			$this->error = PoolError::ERROR_NO_ERRORS;
		} else {
			$this->output = DatabaseError::MSG_ERROR_WRITE_TO_DB_PROBLEM;
			$this->error = DatabaseError::ERROR_WRITE_TO_DB_PROBLEM;
		}
	}
}

?>
