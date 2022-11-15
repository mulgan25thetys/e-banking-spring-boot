 package bank.online.entities;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Proxy;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Entity
@Proxy(lazy=false)
@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@ToString
@Builder
@AllArgsConstructor
public class CarteBancaire implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long idCarteB;
	private String nomBanque = "JMLessous";
	private Boolean estSoucrite;
	private String numero;
	@Temporal(TemporalType.DATE)
	private Date echeance;
	private Long dureeContractuelle;
	@Temporal(TemporalType.DATE)
	private Date dateCreation;
	@Temporal(TemporalType.DATE)
	private Date dateModification;
	
	@OneToOne
	private ContratAssurance contratAssurance;
	
	@ManyToOne
	private ReseauPaiement reseauPaiement;
	
	@OneToOne
	private TypeCarteBancaire typeCarte;

	public CarteBancaire() {
		super();
	}
	

}
