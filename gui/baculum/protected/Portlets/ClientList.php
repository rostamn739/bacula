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

Prado::using('System.Web.UI.ActiveControls.TActiveRepeater');
Prado::using('System.Web.UI.ActiveControls.TActiveDataGrid');
Prado::using('Application.Portlets.ISlideWindow');
Prado::using('Application.Portlets.Portlets');

class ClientList extends Portlets implements ISlideWindow {

	public $ID;
	public $buttonID;
	public $windowTitle;

	public function setID($id) {
		$this->ID = $id;
	}

	public function getID($hideAutoID = true) {
		return $this->ID;
	}

	public function setButtonID($id) {
		$this->buttonID = $id;
	}

	public function getButtonID() {
		return $this->buttonID;
	}

	public function setWindowTitle($param) {
		$this->windowTitle = $param;
	}

	public function getWindowTitle() {
		return $this->windowTitle;
	}

	public function onLoad($param) {
		parent::onLoad($param);
		$this->prepareData();
	}

	public function prepareData($forceReload = false) {
		$allowedButtons = array('ClientBtn', 'ReloadClients');
		if($this->Page->IsPostBack || $this->Page->IsCallBack || $forceReload) {
			if(in_array($this->getPage()->CallBackEventTarget->ID, $allowedButtons) || $forceReload) {
				$params = $this->getUrlParams('clients', $this->getPage()->ClientWindow->ID);
				$clients = $this->Application->getModule('api')->get($params);
				$isDetailView = $_SESSION['view' . $this->getPage()->ClientWindow->ID] == 'details';
				if($isDetailView === true) {
					$this->RepeaterShow->Visible = false;
					$this->DataGridShow->Visible = true;
					$this->DataGrid->DataSource = $this->Application->getModule('misc')->objectToArray($clients->output);
					$this->DataGrid->dataBind();
				} else {
					$this->RepeaterShow->Visible = true;
					$this->DataGridShow->Visible = false;
					$this->Repeater->DataSource = $clients->output;
					$this->Repeater->dataBind();
				}
			}
		}
	}

	public function sortDataGrid($sender, $param) {
		$params = $this->getUrlParams('clients', $this->getPage()->ClientWindow->ID);
		$data = $this->Application->getModule('api')->get($params)->output;
		$data = $this->Application->getModule('misc')->objectToArray($data);
		$this->DataGrid->DataSource = $this->sortData($data, $param->SortExpression, $sender->UniqueID);
		$this->DataGrid->dataBind();
	}

	public function configure($sender, $param) {
		if($this->Page->IsCallBack) {
			$this->getPage()->ClientConfiguration->configure($param->CallbackParameter);
		}
	}
}
?>
