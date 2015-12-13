<?php
/*
 * Bacula(R) - The Network Backup Solution
 * Baculum   - Bacula web interface
 *
 * Copyright (C) 2013-2015 Marcin Haba
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
 
Prado::using('System.Exceptions.TException');
Prado::using('Application.Class.Errors');

abstract class BaculumAPI extends TPage
{
	protected $output;
	protected $error;

	protected $director;

	protected $user;

	/**
	 * Actions methods.
	 */
	const GET_METHOD = 'GET';
	const POST_METHOD = 'POST';
	const PUT_METHOD = 'PUT';
	const DELETE_METHOD = 'DELETE';

	public function onInit($params) {
		parent::onInit($params);
		/*
		 * Workaround to bug in PHP 5.6 by FastCGI that caused general protection error.
		 * TODO: Check on newer PHP if it is already fixed.
		 */
		$db = new ActiveRecord();
		$db->getDbConnection();
		$this->director = isset($this->Request['director']) ? $this->Request['director'] : null;
		$this->user = isset($this->Request['user']) ? $this->Request['user'] : null;
		if(is_null($this->user) && $this->Application->getModule('configuration')->isApplicationConfig() === true) {
			$appConfig = ConfigurationManager::getApplicationConfig();
			// @TOFIX: Baculum API layer should not use $_SERVER variables.
			if (isset($_SERVER['PHP_AUTH_USER'])) {
				// NOTE: With php-fpm $_SERVER['PHP_AUTH_USER'] value is empty string here
				$user = trim($_SERVER['PHP_AUTH_USER']);
				$this->user = (!empty($user) && $user != $appConfig['baculum']['login']) ? $user : null;
			}
		}

		switch($_SERVER['REQUEST_METHOD']) {
			case self::PUT_METHOD: {
				try {
					$this->put();
				} catch(TDbException $e) {
					$this->Application->getModule('logging')->log(__FUNCTION__, $e, Logging::CATEGORY_APPLICATION, __FILE__, __LINE__);
					$this->output = DatabaseError::MSG_ERROR_DB_CONNECTION_PROBLEM;
					$this->error = DatabaseError::ERROR_DB_CONNECTION_PROBLEM;
				}
				break;
			}
			case self::GET_METHOD: {
				try {
					$this->get();
				} catch(TDbException $e) {
					$this->Application->getModule('logging')->log(__FUNCTION__, $e, Logging::CATEGORY_APPLICATION, __FILE__, __LINE__);
					$this->output = DatabaseError::MSG_ERROR_DB_CONNECTION_PROBLEM;
					$this->error = DatabaseError::ERROR_DB_CONNECTION_PROBLEM;
				}
				break;
			}
			case self::POST_METHOD: {
				try {
					$this->post();
				} catch(TDbException $e) {
					$this->Application->getModule('logging')->log(__FUNCTION__, $e, Logging::CATEGORY_APPLICATION, __FILE__, __LINE__);
					$this->output = DatabaseError::MSG_ERROR_DB_CONNECTION_PROBLEM;
					$this->error = DatabaseError::ERROR_DB_CONNECTION_PROBLEM;
				}
				break;
			}
			case self::DELETE_METHOD: {
				try {
					$this->delete();
				} catch(TDbException $e) {
					$this->Application->getModule('logging')->log(__FUNCTION__, $e, Logging::CATEGORY_APPLICATION, __FILE__, __LINE__);
					$this->output = DatabaseError::MSG_ERROR_DB_CONNECTION_PROBLEM;
					$this->error = DatabaseError::ERROR_DB_CONNECTION_PROBLEM;
				}
				break;
			}
		}
	}

	private function getOutput() {
		$output = array('output' => $this->output, 'error' => $this->error);
		return json_encode($output);
	}

	public function onLoad($params) {
		parent::onLoad($params);
		echo $this->getOutput();
	}

	abstract protected function get();

	private function put() {
		$id = isset($this->Request['id']) ? $this->Request['id'] : null;
		if(is_array($this->Request['update']) && count($this->Request['update']) > 0) {
			$params = (object)$this->Request['update'];
			$this->set($id, $params);
		} else {
			$inputstr = file_get_contents("php://input");
			$chunks = explode('&', $inputstr);
			$responseData = array();
			for($i = 0; $i<count($chunks); $i++) {
				parse_str($chunks[$i], $responseEl);
				if(is_array($responseEl) && array_key_exists('update', $responseEl) && is_array($responseEl['update'])) {
					$key = key($responseEl['update']);
					$responseData['update'][$key] = $responseEl['update'][$key];
				}
			}
			if(is_array($responseData) && array_key_exists('update', $responseData)) {
				$params = (object)$responseData['update'];
				$this->set($id, $params);
			} else {
				$this->set($id, array()); //@TOVERIFY
				//$this->output = GenericError::MSG_ERROR_INVALID_COMMAND;
				//$this->error = GenericError::ERROR_INVALID_COMMAND;
			}
		}
	}
	
	private function post() {
		if(is_array($this->Request['create']) && count($this->Request['create']) > 0) {
			$params = (object)$this->Request['create'];
			$this->create($params);
		}
	}

	private function delete() {
		if(isset($this->Request['id'])) {
			$id = intval($this->Request['id']);
			$this->remove($id);
		}
	}

	public function getModule($name) {
		return $this->Application->getModule($name);
	}
}
?>
