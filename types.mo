import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Principal "mo:base/Principal";

module {
	public type Owner = Principal;
	public type Canister = Principal;
	public type  ID = Nat;

	public type Proposal = {
		id: ID;
		proposer: Owner;
		wasm_code:  ?Blob; // valid only for install code type
		ptype: ProposalType;
		canister_id:  ?Canister; // can be null only for create canister case
		approvers: [Owner];
		finished: Bool;
	};

	public type ProposalType = {
		#installCode;
		#uninstallCode;
		#createCanister;
		#startCanister;
		#stopCanister;
		#deleteCanister;
	};

	public func finish_proposer(p1: Proposal) : Proposal {
		{
			id = p1.id;
			proposer = p1.proposer;
			wasm_code = p1.wasm_code;
			ptype = p1.ptype;
			canister_id = p1.canister_id;
			approvers = p1.approvers;
			finished = true;
  		}
	};

	public func add_approver(p1: Proposal, approver: Owner) : Proposal {
		{
			id = p1.id;
			proposer = p1.proposer;
			wasm_code = p1.wasm_code;
			ptype = p1.ptype;
			canister_id = p1.canister_id;
			approvers = Array.append(p1.approvers, [approver]);
			finished = p1.finished;
  		}
	};

	public func update_canister_id(p1: Proposal, id: Canister) : Proposal {
		{
			id = p1.id;
			proposer = p1.proposer;
			wasm_code = p1.wasm_code;
			ptype = p1.ptype;
			canister_id = ?id;
			approvers = p1.approvers;
			finished = p1.finished;
		}
	};
}